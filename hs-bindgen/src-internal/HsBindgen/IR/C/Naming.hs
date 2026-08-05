-- | C naming and declaration identifiers
--
-- This module should only be used within the @HsBindgen.IR@ hierarchy.  From
-- outside the @HsBindgen.IR@ hierarchy, "HsBindgen.IR.C" should be used.
--
-- Within @HsBindgen.IR@, all modules aside from "HsBindgen.IR.C" should import
-- this module qualified for consistency.
--
-- > import HsBindgen.IR.C.Naming qualified as C
module HsBindgen.IR.C.Naming (
    -- * C names
    -- ** Tag kind
    TagKind(..)
  , tagKindPrefix
    -- ** Name kind
  , NameKind(..)
  , checkIsTagged
    -- ** Declaration names
  , DeclName(..)
  , renderDeclName
  , renderDeclNameC
  , parseDeclName
    -- ** Scoped names
  , ScopedName(..)
  , parseScopedName

    -- * PrelimDeclId
  , UnnamedId(..)
  , PrelimDeclId(..)
  , prelimDeclIdSourceName
  , prelimDeclIdNameKind
  , prelimDeclIdAtCursor

    -- * DeclId
  , DeclId(..)
  , declIdSourceName
  , renderNamedDeclId
  , renderDeclId
  , parseDeclId
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  C names

  This is not standard C because we distinguish a separate macro namespace.
-------------------------------------------------------------------------------}

-- | C tag kind for elaborated types (@struct@, @union@, @enum@).
data TagKind = TagKindStruct | TagKindUnion | TagKindEnum
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace TagKind where
  prettyForTrace = PP.show

tagKindPrefix :: TagKind -> Text
tagKindPrefix = \case
    TagKindStruct -> "struct"
    TagKindUnion  -> "union"
    TagKindEnum   -> "enum"

--------------------------------------------------------------------------------

-- | C name kind
--
-- This type distinguishes ordinary names, tagged names, and macro names.  It is
-- needed when the kind is not determined by a context.
data NameKind =
    -- | Ordinary kind
    --
    -- An ordinary name is written without a prefix.
    NameKindOrdinary

    -- | Tagged kind
    --
    -- A tagged name is written with a prefix that specifies the tag kind.
  | NameKindTagged TagKind

    -- | Macro kind
    --
    -- We distinguish a macro name with a @macro@ prefix.  Example: @macro foo@
  | NameKindMacro
  deriving stock (Eq, Generic, Ord, Show)

instance Bounded NameKind where
  minBound = NameKindOrdinary
  maxBound = NameKindMacro

instance Enum NameKind where
  toEnum = \case
    0 -> NameKindOrdinary
    1 -> NameKindTagged TagKindStruct
    2 -> NameKindTagged TagKindUnion
    3 -> NameKindTagged TagKindEnum
    4 -> NameKindMacro
    _ -> panicPure "invalid NameKind toEnum"

  fromEnum = \case
    NameKindOrdinary             -> 0
    NameKindTagged TagKindStruct -> 1
    NameKindTagged TagKindUnion  -> 2
    NameKindTagged TagKindEnum   -> 3
    NameKindMacro                -> 4

instance PrettyForTrace NameKind where
  prettyForTrace = PP.show

checkIsTagged :: NameKind -> Maybe TagKind
checkIsTagged = \case
    NameKindOrdinary       -> Nothing
    NameKindTagged tagKind -> Just tagKind
    NameKindMacro          -> Nothing

--------------------------------------------------------------------------------

-- | C declaration name, qualified by the 'NameKind'
data DeclName = DeclName {
      text :: Text
    , kind :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance IsString DeclName where
  fromString str =
      case parseDeclName (Text.pack str) of
        Just name -> name
        Nothing   -> panicPure $ "invalid DeclName: " ++ show str

instance PrettyForTrace DeclName where
  prettyForTrace = PP.singleQuotes . PP.text . renderDeclName

-- | User-facing syntax for t'DeclName'
renderDeclName :: DeclName -> Text
renderDeclName declName = case declName.kind of
    NameKindOrdinary       -> declName.text
    NameKindTagged tagKind -> tagKindPrefix tagKind <> " " <> declName.text
    NameKindMacro          -> "macro " <> declName.text

-- | C source syntax for t'DeclName'
renderDeclNameC :: DeclName -> Text
renderDeclNameC declName = case declName.kind of
    NameKindOrdinary       -> declName.text
    NameKindTagged tagKind -> tagKindPrefix tagKind <> " " <> declName.text
    NameKindMacro          -> declName.text

-- | Parse a t'DeclName' from 'Text'
parseDeclName :: Text -> Maybe DeclName
parseDeclName t = case Text.words t of
    [n]           -> Just $ DeclName n NameKindOrdinary
    ["struct", n] -> Just $ DeclName n (NameKindTagged TagKindStruct)
    ["union",  n] -> Just $ DeclName n (NameKindTagged TagKindUnion)
    ["enum",   n] -> Just $ DeclName n (NameKindTagged TagKindEnum)
    ["macro",  n] -> Just $ DeclName n NameKindMacro
    _otherwise    -> Nothing

--------------------------------------------------------------------------------

-- | C scoped name
--
-- This is the parsed representation of a C name within a scope.  It is used for
-- field names and function parameter names.
data ScopedName = ScopedName {
      text :: Text
    }
  deriving stock (Eq, Generic, Ord, Show)

instance IsString ScopedName where
  fromString str =
      case parseScopedName (Text.pack str) of
        Just name -> name
        Nothing   -> panicPure $ "invalid DeclName: " ++ show str

instance PrettyForTrace ScopedName where
  prettyForTrace = PP.singleQuotes . PP.text . (.text)

-- | Parse a t'ScopedName' from 'Text'
parseScopedName :: Text -> Maybe ScopedName
parseScopedName t = case Text.words t of
    [n]        -> Just $ ScopedName n
    _otherwise -> Nothing

{-------------------------------------------------------------------------------
  PrelimDeclId
-------------------------------------------------------------------------------}

-- | Unnamed declaration identifier
--
-- A single macro expansion can produce multiple unnamed declarations,
-- and libclang reports the /same/ expansion location for all of them
-- (the macro call site). Without further information they would share an
-- 'UnnamedId'.  Example:
--
-- > #define TwoUntaggedStructs \
-- >     struct { int a; } x; \
-- >     struct { int b; } y;
-- >
-- > TwoUntaggedStructs   // both 'struct {}'s share the expansion location
--
-- The /spelling/ location points back to where each token was originally
-- written -- for macro-expanded code, an offset inside the macro body rather
-- than the call site.  The two structs above have distinct spelling locations
-- (one per @struct@ token in the macro), so keying on it disambiguates them.
--
-- 'loc' (expansion) is what we surface in traces and Haddock, so it stays the
-- human-facing identifier; 'spelling' exists only to make the derived 'Eq' and
-- 'Ord' fine-grained enough.  For non-macro code 'spelling' equals 'loc' and
-- 'UnnamedId' behaves as before.
--
-- The spelling location is only populated correctly on @llvm >= 19.1.0@; on
-- older toolchains it equals the expansion location and the collision
-- returns.
data UnnamedId = UnnamedId {
      -- | Macro expansion site, or the source location for non-macro decls.
      -- Used for tracing and Haddock comments.
      loc      :: SingleLoc
      -- | Spelling location: where the tokens were written in the source
      -- (inside the macro definition, for macro-expanded decls).
    , spelling :: SingleLoc
    , kind     :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace UnnamedId where
  prettyForTrace unnamedId = PP.singleQuotes $ PP.hsep $ [
      "unnamed"
    , case unnamedId.kind of
        NameKindTagged tagKind ->
          PP.text (tagKindPrefix tagKind)
        NameKindOrdinary ->
          PP.empty
        NameKindMacro ->
          "macro"
    , "at"
    , PP.string $ HighLevel.prettySingleLoc ShowFile unnamedId.loc
    ] ++ [
      PP.string $
           "<Spelling="
        ++ HighLevel.prettySingleLoc ShowFile unnamedId.spelling
        ++ ">"
    | unnamedId.spelling /= unnamedId.loc
    ]

--------------------------------------------------------------------------------

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the
-- "HsBindgen.Frontend.Pass.FillUnnamedIds.IsPass.FillUnnamedIds" pass.
data PrelimDeclId =
    -- | Named declaration
    PrelimDeclIdNamed DeclName

    -- | Unnamed declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdUnnamed UnnamedId
  deriving stock (Eq, Ord, Show)

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
    PrelimDeclIdNamed name        -> prettyForTrace name
    PrelimDeclIdUnnamed unnamedId -> prettyForTrace unnamedId

prelimDeclIdSourceName :: PrelimDeclId -> Maybe DeclName
prelimDeclIdSourceName = \case
    PrelimDeclIdNamed  name         -> Just name
    PrelimDeclIdUnnamed  _unnamedId -> Nothing

prelimDeclIdNameKind :: PrelimDeclId -> NameKind
prelimDeclIdNameKind = \case
    PrelimDeclIdNamed name        -> name.kind
    PrelimDeclIdUnnamed unnamedId -> unnamedId.kind

prelimDeclIdAtCursor :: forall m.
     MonadIO m
  => CXCursor
  -> NameKind
  -> m PrelimDeclId
prelimDeclIdAtCursor curr kind = do
    text <- clang_getCursorSpelling curr
    if | Text.null text ->
           -- clang-15 and older use an empty string for unnamed declarations
           markAsUnnamed
       | Text.elem ' ' text ->
           -- clang-16 and newer assign names such as
           --
           -- > struct (unnamed at ....)
           --
           -- /except/ in one case: when we have an untagged struct inside a
           -- typedef, such as
           --
           -- > typedef struct { .. } foo;
           --
           -- newer versions of clang will assign the name @foo@ to the typedef.
           -- This means that in this case we will misclassify the struct as
           -- tagged (and this will then also depend on the clang
           -- version: for older versions we /will/ classify it as untagged).
           -- We smooth over this difference in the
           -- "HsBindgen.Frontend.Pass.FillUnnamedIds" pass (see
           -- "HsBindgen.Frontend.Pass.FillUnnamedIds.ChooseNames").
           markAsUnnamed
       | otherwise ->
           return $ PrelimDeclIdNamed DeclName{text = text, kind = kind}
  where
    markAsUnnamed :: m PrelimDeclId
    markAsUnnamed = do
      cxLoc    <- clang_getCursorLocation curr
      loc      <- HighLevel.clang_getExpansionLocation cxLoc
      spelling <- HighLevel.clang_getSpellingLocation  cxLoc
      return $
        PrelimDeclIdUnnamed UnnamedId{loc = loc, spelling = spelling, kind = kind}

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Identifier for a declaration that appears in the C source
--
-- This is the main ID used throughout @hs-bindgen@ for declarations.
data DeclId = DeclId {
      -- | Name of the declaration
      --
      -- For named declarations, this is /always/ the name as it
      -- appears in the C source; @hs-bindgen@ assigns names to declarations in
      -- the generated /Haskell/ code, and, in particular, does not rename the C
      -- declarations.
      --
      -- For unnamed declarations, this is the name as it is assigned by the
      -- @FillUnnamedIds@ pass, which is also how we then refer to this
      -- declaration in binding specs. The user-facing syntax for untagged
      -- declarations uses an \@-sign in the name; that is not present in the
      -- Haskell value.
      name :: DeclName

      -- | Is this declaration unnamed?
      --
      -- We do /NOT/ record the original unnamed ID here, because that is a source
      -- location, which is impossible to construct in many places (for example,
      -- when parsing @struct \@foo@ in binding specs).
    , isUnnamed :: Bool
    }
  deriving stock (Eq, Ord, Show)

instance PrettyForTrace DeclId where
  prettyForTrace = PP.singleQuotes . PP.text . renderDeclId

declIdSourceName :: DeclId -> Maybe DeclName
declIdSourceName declId = do
    guard $ not declId.isUnnamed
    return declId.name

renderNamedDeclId :: DeclId -> Maybe Text
renderNamedDeclId declId
    | declId.isUnnamed = Nothing
    | otherwise        = Just $ renderDeclName declId.name

-- | User-facing syntax for t'DeclId'
renderDeclId :: DeclId -> Text
renderDeclId declId
    | declId.isUnnamed = renderDeclName $ mapDeclNameText ("@" <>) declId.name
    | otherwise        = renderDeclName declId.name
  where
    mapDeclNameText :: (Text -> Text) -> DeclName -> DeclName
    mapDeclNameText f name = DeclName{text = f name.text, kind = name.kind}

-- | Parse user-facing syntax for t'DeclId'
parseDeclId :: Text -> Maybe DeclId
parseDeclId t = do
    declName <- parseDeclName t
    return $ case Text.uncons declName.text of
      Just ('@', n) -> DeclId{name = DeclName n declName.kind, isUnnamed = True}
      _otherwise    -> DeclId{name = declName, isUnnamed = False}
