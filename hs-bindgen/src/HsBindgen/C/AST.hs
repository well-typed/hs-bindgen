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
  -- todo | TypEnum Enum
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data PrimType =
    PrimInt   -- @int@
  | PrimChar  -- @char@
  | PrimFloat -- @float@
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
