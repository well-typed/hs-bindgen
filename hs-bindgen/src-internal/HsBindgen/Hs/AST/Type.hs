module HsBindgen.Hs.AST.Type (
  HsPrimType (..),
  HsType (..)
) where

import HsBindgen.C.AST qualified as C
import HsBindgen.ExtBindings
import HsBindgen.Imports
import HsBindgen.Hs.AST.Name

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
  deriving stock (Eq, Generic, Show)

data HsType =
    HsPrimType HsPrimType
  | HsTypRef (HsName NsTypeConstr)
  | HsConstArray Natural HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsIO HsType
  | HsFun HsType HsType
  | HsExtBinding ExtIdentifier C.Type
  | HsByteArray
  | HsSizedByteArray Natural Natural
  deriving stock (Generic, Show)

