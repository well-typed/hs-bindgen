-- | Raw AST
--
-- The raw AST is the AST we get from @clang@.
--
-- TODO: Not sure what we can share with the "internal" C AST yet.
module HsBindgen.C.Raw.AST (
    -- * Declarations
    Decl(..)
  , DeclInfo(..)
  , DeclKind(..)
  , Field(..)
    -- * Types (at use sites)
  , Type(..)
    -- ** Primitive types
  , PrimType(..)
  , PrimIntType(..)
  , PrimFloatType(..)
  , PrimSignChar(..)
  , PrimSign(..)
  ) where

import Clang.HighLevel.Types
import HsBindgen.C.Raw.Pass
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  Declarations
-------------------------------------------------------------------------------}

data Decl p = Decl (DeclInfo p) (DeclKind p)

data DeclInfo p = DeclInfo{
      declLoc :: SingleLoc
    , declId  :: Id p
    }

data DeclKind p =
    DeclStruct [Field p]
  | DeclTypedef (Type p)

data Field p = Field{
      fieldName :: Text
    , fieldType :: Type p
    }

deriving stock instance IsPass p => Show (Decl     p)
deriving stock instance IsPass p => Show (DeclInfo p)
deriving stock instance IsPass p => Show (DeclKind p)
deriving stock instance IsPass p => Show (Field    p)

{-------------------------------------------------------------------------------
  Types (at use sites)
-------------------------------------------------------------------------------}

data Type p =
    TypePrim PrimType
  | TypeStruct (Id p)
  | TypeTypedef (Id p)
  | TypePointer (Type p)

data PrimType =
    PrimChar PrimSignChar
  | PrimIntegral PrimIntType PrimSign
  | PrimFloating PrimFloatType
  deriving stock (Show)

data PrimIntType =
    PrimShort
  | PrimInt
  | PrimLong
  | PrimLongLong
  deriving stock (Show)

data PrimFloatType =
    PrimFloat
  | PrimDouble
  | PrimLongDouble
  deriving stock (Show)

data PrimSignChar =
    PrimSignExplicit PrimSign
  | PrimSignImplicit (Maybe PrimSign)
  deriving stock (Show)

data PrimSign = Signed | Unsigned
  deriving stock (Show)

deriving stock instance IsPass p => Show (Type p)
