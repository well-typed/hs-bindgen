-- | Preliminary declaration IDs
--
-- The parser assigns \"preliminary\" IDs to declarations, which are then
-- replaced by the \"real\" IDs (@DeclId@) in the 'HsBindgen.Frontend.Pass.AssignAnonIds.IsPass.AssignAnonIds' pass. They
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

import HsBindgen.Frontend.Naming
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Anonymous declaration identifier
--
-- libclang reports the same expansion location for /every/ declaration
-- produced by a single macro expansion, so two distinct anonymous
-- declarations from one expansion would otherwise share an 'AnonId'. We
-- therefore also key on the spelling location (where the tokens appear in
-- the macro definition).
--
-- The 'spelling' field is rarely read directly. However it is usefult to
-- disambiguate the derived 'Eq' and 'Ord'.
--
-- The spelling location is only populated correctly on @llvm >= 19.1.0@; on
-- older toolchains it equals the expansion location and disambiguation is
-- best-effort.
data AnonId = AnonId{
      -- | Macro expansion site, or the source location for non-macro decls.
      -- Used for tracing and Haddock comments.
      loc      :: SingleLoc
      -- | Spelling location: where the tokens were written in the source
      -- (inside the macro definition, for macro-expanded decls).
    , spelling :: SingleLoc
    , kind     :: CNameKind
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the 'HsBindgen.Frontend.Pass.AssignAnonIds.IsPass.AssignAnonIds' pass.
data PrelimDeclId =
    -- | Named declaration
    Named CDeclName

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | Anon AnonId
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

sourceName :: PrelimDeclId -> Maybe CDeclName
sourceName = \case
    Named  name   -> Just name
    Anon  _anonId -> Nothing

nameKind :: PrelimDeclId -> CNameKind
nameKind = \case
    Named name -> name.kind
    Anon  anon -> anon.kind

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

atCursor :: forall m.
     MonadIO m
  => CXCursor
  -> CNameKind
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
           -- We smooth over this difference in the 'HsBindgen.Frontend.Pass.AssignAnonIds.IsPass.AssignAnonIds' pass
           -- (see "HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames").
           markAsAnon
       | otherwise ->
           return $ Named CDeclName{text = text, kind = kind}
  where
    markAsAnon :: m PrelimDeclId
    markAsAnon = do
        cxLoc    <- clang_getCursorLocation curr
        loc      <- HighLevel.clang_getExpansionLocation cxLoc
        spelling <- HighLevel.clang_getSpellingLocation  cxLoc
        return $ Anon AnonId{loc = loc, spelling = spelling, kind = kind}

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
  prettyForTrace anonId = PP.singleQuotes $ PP.hsep $ [
      "unnamed"
    , case anonId.kind of
        CNameKindTagged tagKind ->
          PP.text (cTagKindPrefix tagKind)
        CNameKindOrdinary ->
          PP.empty
        CNameKindMacro ->
          "macro"
    , "at"
    , PP.string $ HighLevel.prettySingleLoc ShowFile anonId.loc
    ] ++ [
      PP.string $ "<Spelling=" ++ HighLevel.prettySingleLoc ShowFile anonId.spelling ++ ">"
    | anonId.spelling /= anonId.loc
    ]

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
      Named name   -> prettyForTrace name
      Anon  anonId -> prettyForTrace anonId
