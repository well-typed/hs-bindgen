module HsBindgen.Hs.AST.Origin (
    FieldOrigin (..),
    StructOrigin (..),
    EmptyDataOrigin (..),
    NewtypeOrigin (..),
    PatSynOrigin (..),
    ForeignImportDeclOrigin (..),
) where

import HsBindgen.Imports
import HsBindgen.C.AST qualified as C

data FieldOrigin =
      FieldOriginNone
    | FieldOriginStructField C.StructField
  deriving stock (Generic, Show)

data StructOrigin =
      StructOriginStruct C.Struct
    | StructOriginEnum C.Enu
  deriving stock (Generic, Show)

data EmptyDataOrigin =
      EmptyDataOriginOpaqueStruct C.OpaqueStruct
    | EmptyDataOriginOpaqueEnum C.OpaqueEnum
  deriving stock (Generic, Show)

data NewtypeOrigin =
      NewtypeOriginEnum C.Enu
    | NewtypeOriginTypedef C.Typedef
    | NewtypeOriginUnion C.Union
    | NewtypeOriginMacro ( C.Macro C.Ps )
  deriving stock (Generic, Show)

newtype PatSynOrigin =
      PatSynOriginEnumValue C.EnumValue
  deriving stock (Generic, Show)

newtype ForeignImportDeclOrigin =
      ForeignImportDeclOriginFunction C.Function
  deriving stock (Generic, Show)
