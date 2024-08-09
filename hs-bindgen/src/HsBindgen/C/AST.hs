-- | C header AST
--
-- We omit a lot of detail from the full AST, including only information that
-- is relevant for our purposes.
--
-- Intended for qualified import.
--
-- > import HsBindgen.C.AST qualified as C
module HsBindgen.C.AST (
    Header(..)
  , Decl(..)
  , Struct(..)
  , StructField(..)
  , PrimType(..)
  ) where

import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | C header
data Header = Header {
      decls :: [Decl]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Top-level declaration
data Decl =
    DeclStruct Struct
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

-- | Definition of a struct
data Struct = Struct {
      sizeof    :: Int
    , alignment :: Int
    , fields    :: [StructField]
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data StructField = StructField {
      fieldName :: String
    , fieldType :: PrimType
    }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)

data PrimType =
    PrimInt   -- @int@
  | PrimChar  -- @char@
  | PrimFloat -- @float@
  deriving stock (Show, Eq, Generic)
  deriving anyclass (PrettyVal)
