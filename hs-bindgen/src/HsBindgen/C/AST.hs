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
    -- * Types
  , Typ(..)
  , PrimType(..)
  , Typedef(..)
  ) where

import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

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
      fieldName :: String
    , fieldType :: PrimType
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data PrimType =
    PrimInt   -- @int@
  | PrimChar  -- @char@
  | PrimFloat -- @float@
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)
