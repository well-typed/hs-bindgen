module HsBindgen.Hs.AST.Type (
  HsPrimType (..),
  HsType (..),
  hsPrimIntTy,
  hsPrimFloatTy
) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.Haskell

import C.Type qualified

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data HsPrimType
    = HsPrimVoid
    | HsPrimUnit
    | HsPrimCChar
    | HsPrimCSChar
    | HsPrimCUChar
    | HsPrimCInt
    | HsPrimCUInt
    | HsPrimCShort
    | HsPrimCUShort
    | HsPrimCLong
    | HsPrimCULong
    | HsPrimCPtrDiff
    | HsPrimCSize
    -- CWchar
    -- CSigAtomic
    | HsPrimCLLong
    | HsPrimCULLong
    | HsPrimCBool
    -- CIntPtr
    -- CUIntPtr
    -- CIntMax
    -- CUIntMax
    -- CClock
    -- CTime
    -- CUSeconds
    -- CSUSeconds
    | HsPrimCFloat
    | HsPrimCDouble
    | HsPrimCStringLen

    | HsPrimInt

  -- Int8 Int16 Int32 Int64
  -- Word8 Word16 Word32 Word64
  deriving stock (Eq, Ord, Generic, Show)

data HsType =
    HsPrimType HsPrimType
  | HsTypRef (HsName NsTypeConstr)
  | HsConstArray Natural HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsIO HsType
  | HsFun HsType HsType
  | HsExtBinding ExtHsRef BindingSpec.TypeSpec
  | HsByteArray
  | HsSizedByteArray Natural Natural
  | HsBlock HsType
  deriving stock (Generic, Show)

hsPrimIntTy :: C.Type.IntegralType -> HsPrimType
hsPrimIntTy = \case
  C.Type.Bool -> HsPrimCBool
  C.Type.CharLike c ->
    case c of
      C.Type.Char  -> HsPrimCChar
      C.Type.SChar -> HsPrimCSChar
      C.Type.UChar -> HsPrimCUChar
  C.Type.IntLike i ->
    case i of
      C.Type.Short    s ->
        case s of
          C.Type.Signed   -> HsPrimCShort
          C.Type.Unsigned -> HsPrimCUShort
      C.Type.Int      s ->
        case s of
          C.Type.Signed   -> HsPrimCInt
          C.Type.Unsigned -> HsPrimCUInt
      C.Type.Long     s ->
        case s of
          C.Type.Signed   -> HsPrimCLong
          C.Type.Unsigned -> HsPrimCULong
      C.Type.LongLong s ->
        case s of
          C.Type.Signed   -> HsPrimCLLong
          C.Type.Unsigned -> HsPrimCULLong
      C.Type.PtrDiff    -> HsPrimCPtrDiff
      C.Type.Size       -> HsPrimCSize

hsPrimFloatTy :: C.Type.FloatingType -> HsPrimType
hsPrimFloatTy = \case
  C.Type.FloatType  -> HsPrimCFloat
  C.Type.DoubleType -> HsPrimCDouble
