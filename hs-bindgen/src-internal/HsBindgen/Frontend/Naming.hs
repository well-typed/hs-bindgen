-- | C naming and declaration identifiers
--
-- This is such a central module in @hs-bindgen@, it is intended for
-- /unqualified/ import.
--
-- (Historical note: we used to import this qualified as @C@, but that is
-- incorrect: these are @hs-bindgen@ concepts, /not/ standard C concepts. In
-- particular, the names we assign to anonymous declarations is very much
-- @hs-bindgen@ specific.)
module HsBindgen.Frontend.Naming (
    -- * C names
    -- ** Tag kind
    CTagKind(..)
  , cTagKindPrefix
    -- ** Name kind
  , CNameKind(..)
  , checkIsTagged
    -- ** Declaration names
  , CDeclName(..)
  , renderCDeclName
  , renderCDeclNameC
  , parseCDeclName
    -- * Scoped names
  , CScopedName(..)
  , parseCScopedName

    -- * DeclId
  , DeclId(..)
  , declIdSourceName
  , renderDeclId
  , parseDeclId

    -- * Pairing C names and Haskell names
  , ScopedNamePair(..)
  , DeclIdPair(..)
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors (panicPure)
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  C names

  This is not standard C because we distinguish a separate macro namespace.
-------------------------------------------------------------------------------}

-- | C tag kind
--
-- This type distinguishes the kinds of C tags.
data CTagKind =
    -- | @struct@ tag kind
    CTagKindStruct

    -- | @union@ tag kind
  | CTagKindUnion

    -- | @enum@ tag kind
  | CTagKindEnum
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace CTagKind where
  prettyForTrace = PP.show

cTagKindPrefix :: CTagKind -> Text
cTagKindPrefix = \case
    CTagKindStruct -> "struct"
    CTagKindUnion  -> "union"
    CTagKindEnum   -> "enum"

--------------------------------------------------------------------------------

-- | C name kind
--
-- This type distinguishes ordinary names, tagged names, and macro names.  It is
-- needed when the kind is not determined by a context.
data CNameKind =
    -- | Ordinary kind
    --
    -- An ordinary name is written without a prefix.
    CNameKindOrdinary

    -- | Tagged kind
    --
    -- A tagged name is written with a prefix that specifies the tag kind.
  | CNameKindTagged CTagKind

    -- | Macro kind
    --
    -- We distinguish a macro name with a @macro@ prefix.  Example: @macro foo@
  | CNameKindMacro
  deriving stock (Show, Eq, Ord, Generic)

instance Bounded CNameKind where
  minBound = CNameKindOrdinary
  maxBound = CNameKindMacro

instance Enum CNameKind where
  toEnum = \case
    0 -> CNameKindOrdinary
    1 -> CNameKindTagged CTagKindStruct
    2 -> CNameKindTagged CTagKindUnion
    3 -> CNameKindTagged CTagKindEnum
    4 -> CNameKindMacro
    _ -> panicPure "invalid CNameKind toEnum"

  fromEnum = \case
    CNameKindOrdinary              -> 0
    CNameKindTagged CTagKindStruct -> 1
    CNameKindTagged CTagKindUnion  -> 2
    CNameKindTagged CTagKindEnum   -> 3
    CNameKindMacro                 -> 4

instance PrettyForTrace CNameKind where
  prettyForTrace = PP.show

checkIsTagged :: CNameKind -> Maybe CTagKind
checkIsTagged = \case
    CNameKindOrdinary        -> Nothing
    CNameKindTagged cTagKind -> Just cTagKind
    CNameKindMacro           -> Nothing

--------------------------------------------------------------------------------

-- | C declaration name, qualified by the 'CNameKind'
data CDeclName = CDeclName {
      text :: Text
    , kind :: CNameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance IsString CDeclName where
  fromString str =
      case parseCDeclName (Text.pack str) of
        Just name -> name
        Nothing   -> panicPure $ "invalid CDeclName: " ++ show str

instance PrettyForTrace CDeclName where
  prettyForTrace = PP.singleQuotes . PP.text . renderCDeclName

mapCDeclNameText :: (Text -> Text) -> CDeclName -> CDeclName
mapCDeclNameText f name = CDeclName{text = f name.text, kind = name.kind}

-- | User-facing syntax for @CDeclName@
renderCDeclName :: CDeclName -> Text
renderCDeclName cDeclName = case cDeclName.kind of
    CNameKindOrdinary        -> cDeclName.text
    CNameKindTagged cTagKind -> cTagKindPrefix cTagKind <> " " <> cDeclName.text
    CNameKindMacro           -> "macro " <> cDeclName.text

-- | C source syntax for @CDeclName@
renderCDeclNameC :: CDeclName -> Text
renderCDeclNameC cDeclName = case cDeclName.kind of
    CNameKindOrdinary        -> cDeclName.text
    CNameKindTagged cTagKind -> cTagKindPrefix cTagKind <> " " <> cDeclName.text
    CNameKindMacro           -> cDeclName.text

-- | Parse a @CDeclName@ from 'Text'
parseCDeclName :: Text -> Maybe CDeclName
parseCDeclName t = case Text.words t of
    [n]           -> Just $ CDeclName n CNameKindOrdinary
    ["struct", n] -> Just $ CDeclName n (CNameKindTagged CTagKindStruct)
    ["union",  n] -> Just $ CDeclName n (CNameKindTagged CTagKindUnion)
    ["enum",   n] -> Just $ CDeclName n (CNameKindTagged CTagKindEnum)
    ["macro",  n] -> Just $ CDeclName n CNameKindMacro
    _otherwise    -> Nothing

--------------------------------------------------------------------------------

-- | C scoped name
--
-- This is the parsed representation of a C name within a scope.  It is used for
-- field names and function parameter names.
data CScopedName = CScopedName {
      text :: Text
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace CScopedName where
  prettyForTrace = PP.singleQuotes . PP.text . (.text)

-- | Parse a @CScopedName@ from 'Text'
parseCScopedName :: Text -> Maybe CScopedName
parseCScopedName t = case Text.words t of
    [n]        -> Just $ CScopedName n
    _otherwise -> Nothing

{-------------------------------------------------------------------------------
  DeclId
-------------------------------------------------------------------------------}

-- | Identifier for a declaration that appears in the C source
--
-- This is the main ID used throughout @hs-bindgen@ for declarations.
data DeclId = DeclId{
      -- | Name of the declaration
      --
      -- For named (non-anonymous) declarations, this is /always/ the name as it
      -- appears in the C source; @hs-bindgen@ assigns names to declarations in
      -- the generated /Haskell/ code, and, in particular, does not rename the C
      -- declarations.
      --
      -- For anonymous declarations, this is the name as it is assigned by the
      -- @AssignAnonIds@ pass, which is also how we then refer to this
      -- declaration in binding specs. The user-facing syntax for anonymous
      -- declarations uses an \@-sign in the name; that is not present in the
      -- Haskell value.
      name :: CDeclName

      -- | Is this declaration anonymous?
      --
      -- We do /NOT/ record the original anon ID here, because that is a source
      -- location, which is impossible to construct in many places (for example,
      -- when parsing @struct \@foo@ in binding specs).
    , isAnon :: Bool
    }
  deriving stock (Show, Eq, Ord)

declIdSourceName :: DeclId -> Maybe CDeclName
declIdSourceName declId = do
    guard $ not declId.isAnon
    return declId.name

-- | User-facing syntax for @DeclId@
renderDeclId :: DeclId -> Text
renderDeclId declId
  | declId.isAnon = renderCDeclName $ mapCDeclNameText ("@" <>) declId.name
  | otherwise     = renderCDeclName declId.name

-- | Parse user-facing syntax for @DeclId@
parseDeclId :: Text -> Maybe DeclId
parseDeclId t = do
    declName <- parseCDeclName t
    return $ case Text.uncons declName.text of
      Just ('@', n) -> DeclId{name = CDeclName n declName.kind, isAnon = True}
      _otherwise    -> DeclId{name = declName, isAnon = False}

instance PrettyForTrace DeclId where
  prettyForTrace = PP.singleQuotes . PP.text . renderDeclId

{-------------------------------------------------------------------------------
  Pairing C names and Haskell names

  The Haskell name must satisfy the rules for legal Haskell names for the
  intended usage (constructor, variable, ..).
-------------------------------------------------------------------------------}

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1927>
--
-- ScopedNamePair only ever refers to "type constructors" and "variable names".
data ScopedNamePair = ScopedNamePair {
      cName  :: CScopedName
    , hsName :: Hs.SomeName
    }
  deriving stock (Show, Eq, Ord, Generic)

data DeclIdPair = DeclIdPair {
      cName  :: DeclId
    , hsName :: Hs.SomeName
    }
  deriving stock (Show, Eq, Ord)
