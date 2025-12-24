-- | Standard C language types
--
-- Intended for qualified import.
--
-- > import HsBindgen.Language.C qualified as C
module HsBindgen.Language.C (
    -- * C primitive types
    PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSign(..)
  , PrimSignChar(..)
    -- ** Pretty-printing
  , showsPrimType
    -- * C names
    -- ** Tag kind
  , TagKind(..)
  , tagKindPrefix
    -- ** Name kind
  , NameKind(..)
  , checkIsTagged
  , nameKindPrefix
    -- ** Declaration names
  , DeclName(..)
  , mapDeclNameText
  , renderDeclName
  , parseDeclName
    -- ** Scoped names
  , ScopedName(..)
  , parseScopedName
  ) where

import Data.Text qualified as Text
import Text.SimplePrettyPrint qualified as PP

import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.Util.Tracer (PrettyForTrace (prettyForTrace))

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

data PrimType =
    -- | @[signed | unsigned] char@
    PrimChar PrimSignChar

    -- | An integral type, such as @int@ or @unsigned long int@.
  | PrimIntegral PrimIntType PrimSign

    -- | @ptrdiff_t@
  | PrimPtrDiff

    -- | @size_t@
  | PrimSize

    -- | A floating-point type, such as @float@ or @long double@.
  | PrimFloating PrimFloatType

    -- | @_Bool@
  | PrimBool
  deriving stock (Show, Eq, Ord, Generic)

-- | An integral type, such as @int@ or @unsigned long int@.
data PrimIntType
    -- | @[signed | unsigned] short [int]@
  = PrimShort

    -- | @[signed | unsigned] int@
  | PrimInt

    -- | @[signed | unsigned] long [int]@
  | PrimLong

    -- | @[signed | unsigned] long long [int]@
  | PrimLongLong
  deriving stock (Show, Eq, Ord, Enum, Generic)

-- | Primitive floating point types
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/349>
-- We don't currently support @long double@.
data PrimFloatType
    -- | @float@
  = PrimFloat

    -- | @double@
  | PrimDouble
  deriving stock (Show, Eq, Ord, Generic)

-- | Sign of a primitive type
data PrimSign = Signed | Unsigned
  deriving stock (Show, Eq, Ord, Generic)

-- | Sign for @char@
--
-- The C standard distinguishes between /three/ kinds of @char@: @char@, @signed
-- char@ and @unsigned char@. Unlike the other integer types, the interpretation
-- of @char@ as either @signed char@ or @unsigned char@ is implementation
-- defined (see also <https://eel.is/c++draft/basic#fundamental>).
--
-- Our general approach in @hs-bindgen@ is to generate machine code but with a
-- machine independent API. For example, we might know that @int@ is 32 bits on
-- a particular platform, and use this information to define 'sizeOf' in
-- 'Storable' instances, but still use 'CInt' in the type definition (rather
-- than 'Word32'). For this reason, /if/ the sign was compiler inferred, we
-- record this as a special case, so that we can generate 'CChar' instead of
-- 'CUChar' or 'CSChar'.
data PrimSignChar =
    -- ^ User explicitly specified sign
    PrimSignExplicit PrimSign

    -- ^ Sign was left implicit
    --
    -- In most cases we know the compiler-determined sign, but currently not in
    -- all cases. That's probably fixable but it's not trivial; at present we
    -- don't need the information and so we can leave this as a 'Maybe'.
  | PrimSignImplicit (Maybe PrimSign)
  deriving stock (Show, Eq, Ord, Generic)

{-------------------------------------------------------------------------------
  Pretty-printing
-------------------------------------------------------------------------------}

showsPrimType :: PrimType -> ShowS
showsPrimType (PrimChar (PrimSignImplicit _)) = showString "char"
showsPrimType (PrimChar (PrimSignExplicit s)) = showsPrimSign s . showString " char"
showsPrimType (PrimIntegral i s) = showsPrimSign s . showChar ' ' . showsPrimIntType i
showsPrimType (PrimFloating f) = showsPrimFloatType f
showsPrimType PrimPtrDiff = showString "ptrdiff_t"
showsPrimType PrimSize = showString "size_t"
showsPrimType PrimBool = showString "_Bool"

showsPrimIntType :: PrimIntType -> ShowS
showsPrimIntType PrimShort = showString "short"
showsPrimIntType PrimInt = showString "int"
showsPrimIntType PrimLong = showString "long"
showsPrimIntType PrimLongLong = showString "long long"

showsPrimFloatType :: PrimFloatType -> ShowS
showsPrimFloatType PrimFloat = showString "float"
showsPrimFloatType PrimDouble = showString "double"

showsPrimSign :: PrimSign -> ShowS
showsPrimSign Signed = showString "signed"
showsPrimSign Unsigned = showString "unsigned"

{-------------------------------------------------------------------------------
  C names: tag kind
-------------------------------------------------------------------------------}

-- | C tag kind
--
-- This type distinguishes the kinds of C tags.
data TagKind =
    -- | @struct@ tag kind
    TagKindStruct

    -- | @union@ tag kind
  | TagKindUnion

    -- | @enum@ tag kind
  | TagKindEnum
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace TagKind where
  prettyForTrace = PP.showToCtxDoc

tagKindPrefix :: TagKind -> Text
tagKindPrefix = \case
    TagKindStruct -> "struct"
    TagKindUnion  -> "union"
    TagKindEnum   -> "enum"

{-------------------------------------------------------------------------------
  C names: name kind
-------------------------------------------------------------------------------}

-- | C name kind
--
-- This type distinguishes ordinary names and tagged names.  It is needed when
-- the kind is not determined by a context.
data NameKind =
    -- | Ordinary kind
    --
    -- An ordinary name is written without a prefix.
    NameKindOrdinary

    -- | Tagged kind
    --
    -- A tagged name is written with a prefix that specifies the tag kind.
  | NameKindTagged TagKind
  deriving stock (Show, Eq, Ord, Generic)

checkIsTagged :: NameKind -> Maybe TagKind
checkIsTagged = \case
    NameKindOrdinary       -> Nothing
    NameKindTagged tagKind -> Just tagKind

instance Bounded NameKind where
  minBound = NameKindOrdinary
  maxBound = NameKindTagged TagKindEnum

instance Enum NameKind where
  toEnum = \case
    0 -> NameKindOrdinary
    1 -> NameKindTagged TagKindStruct
    2 -> NameKindTagged TagKindUnion
    3 -> NameKindTagged TagKindEnum
    _ -> panicPure "invalid NameKind toEnum"

  fromEnum = \case
    NameKindOrdinary             -> 0
    NameKindTagged TagKindStruct -> 1
    NameKindTagged TagKindUnion  -> 2
    NameKindTagged TagKindEnum   -> 3

instance PrettyForTrace NameKind where
  prettyForTrace = PP.showToCtxDoc

nameKindPrefix :: NameKind -> Maybe Text
nameKindPrefix = \case
    NameKindOrdinary       -> Nothing
    NameKindTagged tagKind -> Just (tagKindPrefix tagKind)

{-------------------------------------------------------------------------------
  C names: declaration names
-------------------------------------------------------------------------------}

-- | C declaration name, qualified by the 'NameKind'
--
-- This is the parsed representation of a @libclang@ C spelling for a
-- declaration.
data DeclName = DeclName {
      text :: Text
    , kind :: NameKind
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace DeclName where
  prettyForTrace = PP.singleQuotes . PP.textToCtxDoc . renderDeclName

mapDeclNameText :: (Text -> Text) -> DeclName -> DeclName
mapDeclNameText f DeclName{text, kind} = DeclName{text = f text, kind}

-- | User-facing syntax for 'DeclName'
renderDeclName :: DeclName -> Text
renderDeclName declName =
    case nameKindPrefix declName.kind of
      Nothing     -> declName.text
      Just prefix -> prefix <> " " <> declName.text

-- | Parse a 'DeclName' from 'Text'
parseDeclName :: Text -> Maybe DeclName
parseDeclName t = case Text.words t of
    [n]           -> Just $ DeclName n NameKindOrdinary
    ["struct", n] -> Just $ DeclName n (NameKindTagged TagKindStruct)
    ["union",  n] -> Just $ DeclName n (NameKindTagged TagKindUnion)
    ["enum",   n] -> Just $ DeclName n (NameKindTagged TagKindEnum)
    _otherwise    -> Nothing

{-------------------------------------------------------------------------------
  C names: scoped names
-------------------------------------------------------------------------------}

-- | C scoped name
--
-- This is the parsed representation of a C name within a scope.  It is used for
-- field names and function parameter names.
data ScopedName = ScopedName {
      text :: Text
    }
  deriving stock (Eq, Generic, Ord, Show)

instance PrettyForTrace ScopedName where
  prettyForTrace = PP.singleQuotes . PP.textToCtxDoc . (.text)

-- | Parse a 'ScopedName' from 'Text'
parseScopedName :: Text -> Maybe ScopedName
parseScopedName t = case Text.words t of
    [n]        -> Just $ ScopedName n
    _otherwise -> Nothing
