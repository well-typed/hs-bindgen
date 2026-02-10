module HsBindgen.Backend.Hs.AST.Type (
  HsPrimType (..),
  HsType (..),
) where

import HsBindgen.Backend.Hs.Name qualified as Hs
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

      -- * Basic FFI types (see 'BasicFFIType')
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

      -- * Foreign.C.Types
    | HsPrimCChar
    | HsPrimCSChar
    | HsPrimCUChar
    | HsPrimCShort
    | HsPrimCUShort
    | HsPrimCInt
    | HsPrimCUInt
    | HsPrimCLong
    | HsPrimCULong
    | HsPrimCLLong
    | HsPrimCULLong
    | HsPrimCBool
    | HsPrimCFloat
    | HsPrimCDouble
  deriving stock (Eq, Ord, Generic, Show)

data HsType =
    HsPrimType HsPrimType
  | HsTypRef (Hs.Name Hs.NsTypeConstr)
      -- | Underlying type (for non-union and non-struct references)
      (Maybe HsType)
  | HsConstArray Natural HsType
  | HsIncompleteArray HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsStablePtr HsType
  | HsPtrConst HsType
  | HsIO HsType
  | HsFun HsType HsType
  | HsExtBinding Hs.ExtRef BindingSpec.CTypeSpec BindingSpec.HsTypeSpec
      -- | Underlying type
      HsType
  | HsByteArray
  | HsSizedByteArray Natural Natural
  | HsBlock HsType
  | HsComplexType HsPrimType
  | HsStrLit String
  | HsWithFlam HsType HsType
  | HsEquivStorable HsType
  deriving stock (Generic, Show, Eq)
