-- | Preliminary declaration IDs
--
-- The parser assigns \"preliminary\" IDs to declarations, which are then
-- replaced by the \"real\" IDs (@DeclId@) in the @AssignAnonIds@ pass. They
-- are different only for anonymous declarations: the preliminary ID for an
-- anonymous declaration is just its source location rather than a name decided
-- based on context (see "HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames").
--
-- Intended for qualified import.
--
-- > import HsBindgen.Frontend.Pass.Parse.PrelimDeclId (PrelimDeclId, AnonId)
-- > import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
module HsBindgen.Frontend.Pass.Parse.PrelimDeclId (
    PrelimDeclId(..)
  , AnonId(..)
    -- * Query
  , sourceName
  , nameKind
    -- * Construction
  , atCursor
  , checkIsBuiltin
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Anonymous declaration identifier
data AnonId = AnonId{
      loc  :: SingleLoc
    , kind :: C.NameKind
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the 'AssignAnonIds' pass.
data PrelimDeclId =
    -- | Named declaration
    Named C.DeclName

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | Anon AnonId
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

sourceName :: PrelimDeclId -> Maybe C.DeclName
sourceName = \case
    Named  name   -> Just name
    Anon  _anonId -> Nothing

nameKind :: PrelimDeclId -> C.NameKind
nameKind = \case
    Named name -> name.kind
    Anon  anon -> anon.kind

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

atCursor :: forall m.
     MonadIO m
  => CXCursor
  -> C.NameKind
  -> m PrelimDeclId
atCursor curr kind = do
    text <- clang_getCursorSpelling curr
    if | Text.null text ->
           -- clang-15 and older use an empty string for anon declarations
           markAsAnon
       | Text.elem ' ' text ->
           -- clang-16 and newer assign names such as
           --
           -- > struct (unnamed at ....)
           --
           -- /except/ in one case: when we have an anonymous struct inside a
           -- typedef, such as
           --
           -- > typedef struct { .. } foo;
           --
           -- newer versions of clang will assign the name @foo@ to the typedef.
           -- This means that in this case we will misclassify the struct as
           -- not-anonymous (and this will then also depend on the clang
           -- version: for older versions we /will/ classify it as anonymous).
           -- We smooth over this difference in the @AssignAnonIds@ pass
           -- (see "HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames").
           markAsAnon
       | otherwise ->
           return $ Named C.DeclName{text = text, kind = kind}
  where
    markAsAnon :: m PrelimDeclId
    markAsAnon = do
        loc <- HighLevel.clang_getCursorLocation' curr
        return $ Anon AnonId{loc = loc, kind = kind}

-- | Check for built-in definitions
checkIsBuiltin :: MonadIO m => CXCursor -> m (Maybe Text)
checkIsBuiltin curr = do
    mRange <- clang_Cursor_getSpellingNameRange curr 0 0
    case mRange of
      Nothing    -> return Nothing
      Just range -> do
        start <- clang_getRangeStart range
        (file, _col, _line, _offset) <- clang_getExpansionLocation start
        if isNullPtr file
          then Just <$> clang_getCursorSpelling curr
          else return Nothing

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

instance PrettyForTrace AnonId where
  prettyForTrace anonId = PP.singleQuotes $ PP.hsep [
      "unnamed"
    , case anonId.kind of
        C.NameKindTagged tagKind ->
          PP.text (C.tagKindPrefix tagKind)
        C.NameKindOrdinary ->
          PP.empty
    , "at"
    , PP.string $ HighLevel.prettySingleLoc ShowFile anonId.loc
    ]

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
      Named name   -> prettyForTrace name
      Anon  anonId -> prettyForTrace anonId
