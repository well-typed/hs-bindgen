module HsBindgen.Backend.Hs.AST.Type (
  HsPrimType (..),
  HsType (..),
) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data HsPrimType =
      -- * Others
      HsPrimVoid
    | HsPrimUnit
    | HsPrimCStringLen

      -- * Basic foreign types (see 'BasicForeignType')
    | HsPrimChar
    | HsPrimInt
    | HsPrimDouble
    | HsPrimFloat
    | HsPrimBool
    | HsPrimInt8
    | HsPrimInt16
    | HsPrimInt32
    | HsPrimInt64
    | HsPrimWord
    | HsPrimWord8
    | HsPrimWord16
    | HsPrimWord32
    | HsPrimWord64

      -- * Builtin foreign types (see 'BuiltinForeignType')
    | HsPrimIntPtr
    | HsPrimWordPtr
    | HsPrimCChar
    | HsPrimCSChar
    | HsPrimCUChar
    | HsPrimCShort
    | HsPrimCUShort
    | HsPrimCInt
    | HsPrimCUInt
    | HsPrimCLong
    | HsPrimCULong
    | HsPrimCPtrdiff
    | HsPrimCSize
    | HsPrimCWchar
    | HsPrimCSigAtomic
    | HsPrimCLLong
    | HsPrimCULLong
    | HsPrimCBool
    | HsPrimCIntPtr
    | HsPrimCUIntPtr
    | HsPrimCIntMax
    | HsPrimCUIntMax
    | HsPrimCClock
    | HsPrimCTime
    | HsPrimCUSeconds
    | HsPrimCSUSeconds
    | HsPrimCFloat
    | HsPrimCDouble
  deriving stock (Eq, Ord, Generic, Show)

data HsType =
    HsPrimType HsPrimType
  | HsTypRef (Hs.Name Hs.NsTypeConstr)
  | HsConstArray Natural HsType
  | HsIncompleteArray HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsStablePtr HsType
  | HsConstPtr HsType
  | HsIO HsType
  | HsFun HsType HsType
  | HsExtBinding Hs.ExtRef BindingSpec.CTypeSpec (Maybe BindingSpec.HsTypeSpec)
  | HsByteArray
  | HsSizedByteArray Natural Natural
  | HsBlock HsType
  | HsComplexType HsPrimType
  | HsStrLit String
  deriving stock (Generic, Show, Eq)

