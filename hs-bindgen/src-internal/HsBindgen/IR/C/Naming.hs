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
  , ScopedNamePair(..)

    -- * PrelimDeclId
  , AnonId(..)
  , PrelimDeclId(..)
  , prelimDeclIdSourceName
  , prelimDeclIdNameKind
  , prelimDeclIdAtCursor

    -- * DeclId
  , DeclId(..)
  , declIdSourceName
  , renderNonAnonDeclId
  , renderDeclId
  , parseDeclId
  , DeclIdPair(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import Clang.HighLevel (ShowFile (..))
import Clang.HighLevel qualified as HighLevel
import Clang.HighLevel.Types
import Clang.LowLevel.Core

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
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

instance PrettyForTrace ScopedName where
  prettyForTrace = PP.singleQuotes . PP.text . (.text)

-- | Parse a t'ScopedName' from 'Text'
parseScopedName :: Text -> Maybe ScopedName
parseScopedName t = case Text.words t of
    [n]        -> Just $ ScopedName n
    _otherwise -> Nothing

--------------------------------------------------------------------------------

-- | A t'ScopedName' paired with a Haskell name
data ScopedNamePair = ScopedNamePair {
      cName  :: ScopedName
      -- TODO <https://github.com/well-typed/hs-bindgen/issues/1927>
      -- ScopedNamePair only ever refers to type constructors and variable
      -- names.
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Generic, Ord, Show)

{-------------------------------------------------------------------------------
  PrelimDeclId
-------------------------------------------------------------------------------}

-- | Anonymous declaration identifier
--
-- A single macro expansion can produce multiple anonymous tag declarations,
-- and libclang reports the /same/ expansion location for all of them
-- (the macro call site). Without further information they would share an
-- 'AnonId'.  Example:
--
-- > #define TwoAnons \
-- >     struct { int a; } x; \
-- >     struct { int b; } y;
-- >
-- > TwoAnons   // both 'struct {}'s share the expansion location
--
-- The /spelling/ location points back to where each token was originally
-- written -- for macro-expanded code, an offset inside the macro body rather
-- than the call site.  The two structs above have distinct spelling locations
-- (one per @struct@ token in the macro), so keying on it disambiguates them.
--
-- 'loc' (expansion) is what we surface in traces and Haddock, so it stays the
-- human-facing identifier; 'spelling' exists only to make the derived 'Eq' and
-- 'Ord' fine-grained enough.  For non-macro code 'spelling' equals 'loc' and
-- 'AnonId' behaves as before.
--
-- The spelling location is only populated correctly on @llvm >= 19.1.0@; on
-- older toolchains it equals the expansion location and the collision
-- returns.
data AnonId = AnonId {
      -- | Macro expansion site, or the source location for non-macro decls.
      -- Used for tracing and Haddock comments.
      loc      :: SingleLoc
      -- | Spelling location: where the tokens were written in the source
      -- (inside the macro definition, for macro-expanded decls).
    , spelling :: SingleLoc
    , kind     :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace AnonId where
  prettyForTrace anonId = PP.singleQuotes $ PP.hsep $ [
      "unnamed"
    , case anonId.kind of
        NameKindTagged tagKind ->
          PP.text (tagKindPrefix tagKind)
        NameKindOrdinary ->
          PP.empty
        NameKindMacro ->
          "macro"
    , "at"
    , PP.string $ HighLevel.prettySingleLoc ShowFile anonId.loc
    ] ++ [
      PP.string $
           "<Spelling="
        ++ HighLevel.prettySingleLoc ShowFile anonId.spelling
        ++ ">"
    | anonId.spelling /= anonId.loc
    ]

--------------------------------------------------------------------------------

-- | Preliminary declaration identifier
--
-- Not all declarations in a C header have names; to be able to nonetheless
-- refer to these declarations we use the source location.  We replace these by
-- proper names in the
-- "HsBindgen.Frontend.Pass.AssignAnonIds.IsPass.AssignAnonIds" pass.
data PrelimDeclId =
    -- | Named declaration
    PrelimDeclIdNamed DeclName

    -- | Anonymous declaration
    --
    -- This can only happen for tagged types: structs, unions and enums
  | PrelimDeclIdAnon AnonId
  deriving stock (Eq, Ord, Show)

instance PrettyForTrace PrelimDeclId where
  prettyForTrace = \case
    PrelimDeclIdNamed name   -> prettyForTrace name
    PrelimDeclIdAnon  anonId -> prettyForTrace anonId

prelimDeclIdSourceName :: PrelimDeclId -> Maybe DeclName
prelimDeclIdSourceName = \case
    PrelimDeclIdNamed  name   -> Just name
    PrelimDeclIdAnon  _anonId -> Nothing

prelimDeclIdNameKind :: PrelimDeclId -> NameKind
prelimDeclIdNameKind = \case
    PrelimDeclIdNamed name -> name.kind
    PrelimDeclIdAnon  anon -> anon.kind

prelimDeclIdAtCursor :: forall m.
     MonadIO m
  => CXCursor
  -> NameKind
  -> m PrelimDeclId
prelimDeclIdAtCursor curr kind = do
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
           -- We smooth over this difference in the
           -- "HsBindgen.Frontend.Pass.AssignAnonIds" pass (see
           -- "HsBindgen.Frontend.Pass.AssignAnonIds.ChooseNames").
           markAsAnon
       | otherwise ->
           return $ PrelimDeclIdNamed DeclName{text = text, kind = kind}
  where
    markAsAnon :: m PrelimDeclId
    markAsAnon = do
      cxLoc    <- clang_getCursorLocation curr
      loc      <- HighLevel.clang_getExpansionLocation cxLoc
      spelling <- HighLevel.clang_getSpellingLocation  cxLoc
      return $
        PrelimDeclIdAnon AnonId{loc = loc, spelling = spelling, kind = kind}

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Identifier for a declaration that appears in the C source
--
-- This is the main ID used throughout @hs-bindgen@ for declarations.
data DeclId = DeclId {
      -- | Name of the declaration
      --
      -- For named (non-anonymous) declarations, this is /always/ the name as it
      -- appears in the C source; @hs-bindgen@ assigns names to declarations in
      -- the generated /Haskell/ code, and, in particular, does not rename the C
      -- declarations.
      --
      -- For anonymous declarations, this is the name as it is assigned by the
      -- @AssignAnonIds@ pass, which is also how we then refer to this
      -- declaration in binding specs. The user-facing syntax for untagged
      -- declarations uses an \@-sign in the name; that is not present in the
      -- Haskell value.
      name :: DeclName

      -- | Is this declaration anonymous?
      --
      -- We do /NOT/ record the original anon ID here, because that is a source
      -- location, which is impossible to construct in many places (for example,
      -- when parsing @struct \@foo@ in binding specs).
    , isAnon :: Bool
    }
  deriving stock (Eq, Ord, Show)

instance PrettyForTrace DeclId where
  prettyForTrace = PP.singleQuotes . PP.text . renderDeclId

declIdSourceName :: DeclId -> Maybe DeclName
declIdSourceName declId = do
    guard $ not declId.isAnon
    return declId.name

renderNonAnonDeclId :: DeclId -> Maybe Text
renderNonAnonDeclId declId
    | declId.isAnon = Nothing
    | otherwise     = Just $ renderDeclName declId.name

-- | User-facing syntax for t'DeclId'
renderDeclId :: DeclId -> Text
renderDeclId declId
    | declId.isAnon = renderDeclName $ mapDeclNameText ("@" <>) declId.name
    | otherwise     = renderDeclName declId.name
  where
    mapDeclNameText :: (Text -> Text) -> DeclName -> DeclName
    mapDeclNameText f name = DeclName{text = f name.text, kind = name.kind}

-- | Parse user-facing syntax for t'DeclId'
parseDeclId :: Text -> Maybe DeclId
parseDeclId t = do
    declName <- parseDeclName t
    return $ case Text.uncons declName.text of
      Just ('@', n) -> DeclId{name = DeclName n declName.kind, isAnon = True}
      _otherwise    -> DeclId{name = declName, isAnon = False}

--------------------------------------------------------------------------------

-- | A t'DeclId' paired with a Haskell name
data DeclIdPair = DeclIdPair {
      cName  :: DeclId
    , hsName :: Hs.SomeName
    }
  deriving stock (Eq, Ord, Show)
