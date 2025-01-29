module HsBindgen.Hs.AST.Type (
  HsPrimType (..),
  HsType (..)
) where

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
    -- CSize
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

  -- Int8 Int16 Int32 Int64
  -- Word8 Word16 Word32 Word64
  deriving stock (Eq, Generic, Show)

data HsType =
    HsType String
  | HsPrimType HsPrimType
  | HsTypRef (HsName NsTypeConstr)
  | HsConstArray Natural HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsIO HsType
  | HsFun HsType HsType
  deriving stock (Generic, Show)

