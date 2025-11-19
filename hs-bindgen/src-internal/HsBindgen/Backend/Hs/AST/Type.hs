module HsBindgen.Backend.Hs.AST.Type (
  HsPrimType (..),
  HsType (..),
  ResultType(..),
  extractResultType,
) where

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

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
  | HsTypRef (Hs.Name Hs.NsTypeConstr)
  | HsConstArray Natural HsType
  | HsIncompleteArray HsType
  | HsPtr HsType
  | HsFunPtr HsType
  | HsIO HsType
  | HsFun HsType HsType
  | HsExtBinding Hs.ExtRef BindingSpec.CTypeSpec (Maybe BindingSpec.HsTypeSpec)
  | HsByteArray
  | HsSizedByteArray Natural Natural
  | HsBlock HsType
  | HsComplexType HsPrimType
  | HsStrLit String
  deriving stock (Generic, Show, Eq)

-- | When translating a 'C.Type' there are C types which we
-- cannot pass directly using C FFI. We need to distinguish these.
--
-- Result types can be heap types, which are types we can't return by value
-- due to Haskell FFI limitation. Or they can be normal types supported by
-- Haskell FFI. This is also true for function arguments as well, result types
-- are a special case where unsupported result types become arguments.
--
data ResultType a =
    NormalResultType a
    -- ^ Normal result type.
  | HeapResultType a
    -- ^ Heap type that is not supported by Haskell FFI
  deriving stock (Generic, Show, Functor)

extractResultType :: ResultType a -> a
extractResultType (NormalResultType t) = t
extractResultType (HeapResultType t)   = t

