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
  , OrigDeclId(..)
  , unsafeDeclIdHaskellName
  , declIdCName

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
import HsBindgen.Frontend.Pass
import HsBindgen.Imports
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  AnonId
-------------------------------------------------------------------------------}

-- | Anonymous declaration identifier
newtype AnonId = AnonId SingleLoc
  deriving stock (Show, Eq, Ord, Generic)

-- | We mimic the syntax used by Clang itself for anonymous declarations
instance PrettyForTrace AnonId where
  prettyForTrace (AnonId loc) = PP.string $
    "unnamed at " ++ HighLevel.prettySingleLoc ShowFile loc

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
  | PrelimDeclIdAnon AnonId C.NameKind
  deriving stock (Show, Eq, Ord)

prelimDeclIdName :: PrelimDeclId -> Maybe C.DeclName
prelimDeclIdName = \case
    PrelimDeclIdNamed  name         -> Just name
    PrelimDeclIdAnon  _anonId _kind -> Nothing

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
      PrelimDeclIdNamed n ->
        prettyForTrace n
      PrelimDeclIdAnon anonId kind ->
        PP.singleQuotes $
          case kind of
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
        return $ PrelimDeclIdAnon (AnonId loc) nameKind

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
-------------------------------------------------------------------------------}

-- | Declaration identifier
--
-- All declarations have names after renaming in the @NameAnon@ pass.  This type
-- is used until the @MangleNames@ pass.
data DeclId (p :: Pass) = DeclId{
      name       :: C.DeclName
    , origDeclId :: OrigDeclId
    , haskellId  :: HaskellId p
    }

deriving instance Show (HaskellId p) => Show (DeclId p)
deriving instance Eq   (HaskellId p) => Eq   (DeclId p)
deriving instance Ord  (HaskellId p) => Ord  (DeclId p)

data OrigDeclId =
    -- | The original ID (as it appeared in the C source)
    OrigDeclId PrelimDeclId

    -- | Auxiliary declaration
    --
    -- In some cases we introduce auxiliary declarations that do not exist in
    -- the C source. In this case, we record which original declaration this
    -- declaration is in support of.
  | AuxForDecl PrelimDeclId
  deriving stock (Show, Eq, Ord)

-- | Construct name in arbitrary name space
--
-- The caller must ensure that name rules are adhered to.
unsafeDeclIdHaskellName :: HaskellId p ~ Hs.Identifier => DeclId p -> Hs.Name ns
unsafeDeclIdHaskellName declId =
    case declId.haskellId of
      Hs.Identifier name -> Hs.Name name

-- | How do we refer to this declaration in C code?
declIdCName :: DeclId p -> Maybe C.DeclName
declIdCName declId =
   case declId.origDeclId of
     AuxForDecl _parent -> Nothing
     OrigDeclId orig    ->
       case orig of
         PrelimDeclIdNamed  name -> Just name
         PrelimDeclIdAnon{}      -> Nothing

instance PrettyForTrace (DeclId p) where
   prettyForTrace declId = PP.hsep [
         prettyForTrace declId.name
       , case declId.origDeclId of
           OrigDeclId orig | prelimDeclIdName orig /= Just declId.name ->
             PP.parens (prettyForTrace orig)
           _otherwise ->
             PP.empty
       ]

instance PrettyForTrace (Located (DeclId p)) where
  prettyForTrace (Located loc declId) =
      prettyForTraceLoc declId loc

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
