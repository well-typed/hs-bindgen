-- | C header AST
--
-- We omit a lot of detail from the full AST, including only information that
-- is relevant for our purposes.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.AST qualified as C
module HsBindgen.C.AST (
    -- * Top-level
    Header(..)
  , Decl(..)
    -- * Structs
  , Struct(..)
  , StructField(..)
    -- * Enums
  , Enu(..)
  , EnumValue(..)
    -- * Types
  , Typ(..)
  , PrimType(..)
  , PrimSign(..)
  , Typedef(..)
    -- * Macros
  , Token(..)
  , Macro(..)
  ) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal(..))

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

-- | C header
data Header = Header {
      headerDecls :: [Decl]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Top-level declaration
data Decl =
    DeclStruct Struct
  | DeclTypedef Typedef
  | DeclEnum Enu
  | DeclMacro Macro
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Structs
-------------------------------------------------------------------------------}

-- | Definition of a struct
data Struct = Struct {
      structTag       :: Maybe String
    , structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data StructField = StructField {
      fieldName   :: String
    , fieldOffset :: Int
    , fieldType   :: Typ
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Enums
-------------------------------------------------------------------------------}

data Enu = Enu {
      enumTag       :: Maybe String
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumValues    :: [EnumValue]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data EnumValue = EnumValue {
      valueName  :: String
    , valueValue :: Integer
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Typedefs
-------------------------------------------------------------------------------}

data Typedef = Typedef {
      typedefName :: String
    , typedefType :: Typ
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Typ =
    TypPrim PrimType
  | TypStruct Struct
  | TypPointer Typ
  | TypElaborated -- TODO: reference into type symbol table.
  -- todo | TypEnum Enum
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Primitive type
--
-- The interpretation of the primitive types is in many cases implementation
-- and/or machine dependent. In @hs-bindgen@ we are dealing with one specific
-- implementation (@libclang@), and we are generating code for one specific
-- machine (possibly cross platform). This means we have a choice; suppose we
-- see a field of type @int@ in a C struct:
--
-- 1. We could produce a field of type 'CInt' in the generated Haskell code
-- 2. We could query @libclang@ to what choice it makes for the selected
--    target platform, and use 'CShort' or 'CLong' (or something else again.
--
-- Both options have advantages; most users will probably prefer (1), so that
-- we generate a /single/ API, independent of implementation details. However,
-- some users may prefer (2) in some cases, if they want to take advantage of
-- specific features of the target platform.
--
-- We don't force the decision here, but simply represent the C AST faithfully.
data PrimType =
    -- | @[signed | unsigned] char@
    --
    -- The C standard distinguishes between /three/ kinds of @cha@: @char@,
    -- @signed char@ and @unsigned char@. Unlike the other integer types,
    -- the interpretation of @char@ as either @signed char@ or @unsigned char@
    -- is implementation defined.
    --
    -- See also <https://eel.is/c++draft/basic#fundamental>.
    PrimChar (Maybe PrimSign)

    -- | @[signed | unsigned] short [int]@
  | PrimShortInt PrimSign

    -- | @[signed | unsigned] int@
  | PrimInt PrimSign

    -- | @[signed | unsigned] long [int]@
  | PrimLong PrimSign

    -- | @[signed | unsigned] long long [int]@
  | PrimLongLong PrimSign

    -- | @float@
  | PrimFloat

    -- | @double@
  | PrimDouble

    -- | @long double@
  | PrimLongDouble
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Sign of a primitive type
data PrimSign = Signed | Unsigned
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

{-------------------------------------------------------------------------------
  Macros
-------------------------------------------------------------------------------}

newtype Token = Token ByteString
  deriving stock (Show, Eq, Generic)

instance PrettyVal Token where
  prettyVal (Token t) = prettyVal (show t)

-- | C macro definition
--
-- This is simply the list of tokens; for example,
--
-- > #define MYFOO 1
-- > #define INCR(x) x + 1
--
-- are represented as
--
-- > Macro ["MYFOO", "1"]
-- > Macro ["INCR", "(", "x", ")", "x", "+", "1"]
--
-- respectively.
data Macro = Macro [Token]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)
