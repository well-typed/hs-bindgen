-- | C naming and declaration identifiers
--
-- Intended for qualified import within frontend.
--
-- > import HsBindgen.Frontend.Naming qualified as C
--
-- Use outside of the frontend should be done via
-- "HsBindgen.Frontend.AST.External".
module HsBindgen.Frontend.Naming (
    -- * AnonId
    AnonId(..)

    -- * PrelimDeclId
  , PrelimDeclId(..)
  , prelimDeclIdName
  , getPrelimDeclId
  , checkIsBuiltin

    -- * DeclId
  , DeclId(..)
  , renderDeclId
  , parseDeclId

    -- * DeclIdPair
  , DeclIdPair(..)

    -- * Located
  , Located(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint (CtxDoc, (<+>))
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Anonymous declaration identifier
data AnonId = AnonId{
      loc  :: SingleLoc
    , kind :: C.NameKind
    }
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimic the syntax used by Clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace anonId = PP.string $
    "unnamed at " ++ HighLevel.prettySingleLoc ShowFile anonId.loc

{-------------------------------------------------------------------------------
  PrelimDeclId
-------------------------------------------------------------------------------}

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the 'NameAnon' pass.
data PrelimDeclId =
    -- | Named declaration
    PrelimDeclIdNamed C.DeclName

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdAnon AnonId
  deriving stock (Show, Eq, Ord)

prelimDeclIdName :: PrelimDeclId -> Maybe C.DeclName
prelimDeclIdName = \case
    PrelimDeclIdNamed  name   -> Just name
    PrelimDeclIdAnon  _anonId -> Nothing

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
      PrelimDeclIdNamed n ->
        prettyForTrace n
      PrelimDeclIdAnon anonId ->
        PP.singleQuotes $
          case anonId.kind of
            C.NameKindTagged kind' ->
                  PP.textToCtxDoc (C.tagKindPrefix kind')
              <+> PP.parens (prettyForTrace anonId)
            C.NameKindOrdinary ->
              panicPure "unexpected anonymous ordinary name"

instance PrettyForTrace (Located PrelimDeclId) where
  prettyForTrace (Located l i) =
      case i of
        PrelimDeclIdNamed n ->
          prettyForTraceLoc n l
        PrelimDeclIdAnon{}  ->
          -- No need to repeat the source location in this case
          prettyForTrace i

getPrelimDeclId :: forall m.
     MonadIO m
  => CXCursor
  -> C.NameKind
  -> m PrelimDeclId
getPrelimDeclId curr nameKind = do
    spelling  <- clang_getCursorSpelling curr
    if | Text.null spelling ->
         -- clang-15 and older use an empty string for anon declarations
         markAsAnon
       | Text.elem ' ' spelling ->
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
         -- not-anonymous (and this will then also depend on the clang version:
         -- for older versions we /will/ classify it as anonymous). However,
         -- since we squash structs with a single use site inside a typedef
         -- anyway, this misclassification does not matter.
         markAsAnon
       | otherwise ->
         return $ PrelimDeclIdNamed (C.DeclName spelling nameKind)
  where
    markAsAnon :: m PrelimDeclId
    markAsAnon = do
        loc <- HighLevel.clang_getCursorLocation' curr
        return $ PrelimDeclIdAnon (AnonId loc nameKind)

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
  DeclId

  TODO: .Naming module should go.
-------------------------------------------------------------------------------}

-- | Identifier for a declaration that appears in the C source
data DeclId = DeclId{
      -- | Name of the declaration
      --
      -- For named (non-anonymous) declarations, this is /always/ the name as it
      -- appears in the C source; any renaming of declarations we do in
      -- @hs-bindgen@ happens in the generated /Haskell/ code, not the C
      -- declarations.
      --
      -- For anonymous declarations, this is the name as it is assigned by the
      -- @AssignAnonIds@ pass, which is also how we then refer to this
      -- declaration in binding specs. The user-facing syntax for anonymous
      -- declarations uses an \@-sign in the name; that is not present in the
      -- Haskell value.
      name :: C.DeclName

      -- | Is this declaration anonymous?
      --
      -- We do /NOT/ record the original anon ID here, because that is a source
      -- location, which is impossible to construct in many places (for example,
      -- when parsing @struct \@foo@ in binding specs).
    , isAnon :: Bool
    }
  deriving stock (Show, Eq, Ord)

-- | User-facing syntax for 'DeclId'
renderDeclId :: DeclId -> Text
renderDeclId declId
  | declId.isAnon = C.renderDeclName $ C.mapDeclNameText ("@" <>) declId.name
  | otherwise     = C.renderDeclName declId.name

-- | Parse user-facing syntax for 'DeclId'
parseDeclId :: Text -> Maybe DeclId
parseDeclId t = do
    declName <- C.parseDeclName t
    return $ case Text.uncons declName.text of
      Just ('@', n) -> DeclId{name = C.DeclName n declName.kind, isAnon = True}
      _otherwise    -> DeclId{name = declName, isAnon = False}

instance PrettyForTrace DeclId where
  prettyForTrace = PP.singleQuotes . PP.textToCtxDoc . renderDeclId

instance PrettyForTrace (Located DeclId) where
  prettyForTrace (Located loc declId) =
      prettyForTraceLoc declId loc

{-------------------------------------------------------------------------------
  DeclIdPair
-------------------------------------------------------------------------------}

data DeclIdPair = DeclIdPair{
      cName  :: DeclId
    , hsName :: Hs.Identifier
    }
  deriving stock (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Located
-------------------------------------------------------------------------------}

-- | Indirection for 'PrettyForTrace' instance for @DeclInfo@
--
-- By introducing this auxiliary type, used in the 'PrettyForTrace' instance
-- for @DeclInfo@, we delegate to @Id p@ instances.
data Located a = Located SingleLoc a

prettyForTraceLoc :: PrettyForTrace a => a -> SingleLoc -> CtxDoc
prettyForTraceLoc x l = PP.hsep [
      prettyForTrace x
    , "at"
    , PP.showToCtxDoc l
    ]
