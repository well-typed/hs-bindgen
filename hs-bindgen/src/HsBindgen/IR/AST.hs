module HsBindgen.IR.AST (
    -- * Names
    TypedefName (..),
    StructName (..),
    EnumName (..),
    -- * Declrations
    Decl (..),
    -- * Structs
    StructDefn (..),
    StructField (..),
    -- * Enums
    EnumDefn (..),
    EnumValue (..),
    -- * Types
    Type (..),
) where

import HsBindgen.Imports
import HsBindgen.C.AST.Type (PrimType)
import HsBindgen.C.AST.Name (CName)
import HsBindgen.C.AST (MacroDecl (..))

-- | Name of a typedef
newtype TypedefName = TypedefName Text
  deriving stock Show

-- | Name of a struct, @foo@ in @struct foo@
newtype StructName = StructName Text
  deriving stock Show

-- | Name of an enum, @bar@ in @enum bar@
newtype EnumName = EnumName Text
  deriving stock Show

data Decl
    = DeclTypedef TypedefName Type
    | DeclStruct StructName StructDefn
    | DeclEnum EnumName EnumDefn
    | DeclMacro MacroDecl
  deriving stock Show

data StructDefn = StructDefn
    { structSizeof    :: Int
    , structAlignment :: Int
    , structFields    :: [StructField]
    }
  deriving stock Show

data StructField = StructField {
      fieldName   :: CName
    , fieldOffset :: Int -- ^ Offset in bits
    , fieldType   :: Type
    }
  deriving stock (Show)

data EnumDefn = EnumDefn
    { enumType      :: Type
    , enumSizeof    :: Int
    , enumAlignment :: Int
    , enumValues    :: [EnumValue]
    }
  deriving stock (Show)

data EnumValue = EnumValue
    { valueName  :: CName
    , valueValue :: Integer
    }
  deriving stock (Show)

-- | Type (a use-site of).
data Type
    = TypePrim PrimType
    | TypeStruct StructName
    | TypeTypedef TypedefName
    | TypeEnum EnumName
    | TypeConstArray Natural Type
    | TypePointer Type
  deriving stock Show
