-- | Datatypes for the 'HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType'
-- class.
module HsBindgen.Runtime.BaseForeignType (
    BaseForeignType (..)
  , BasicForeignType (..)
  , BuiltinForeignType (..)
  ) where

-- | A foreign type without newtypes
--
-- This datatype does not restrict itself to true foreign types. For example,
-- @IO Unit `FunArrow` IO Unit@ is not a foreign type, but we can represent it
-- here. See the discussion on the
-- 'HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType' class for why this
-- is okay.
--
-- Also, see the "8.4.2 Foreign Types" section of the report for more
-- information about foreign types:
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
data BaseForeignType =
    -- === Foreign types ===
    FunArrow BaseForeignType BaseForeignType

    -- === Marshallable foreign result types ===
  | Unit
  | IO BaseForeignType

    -- === Marshallable foreign types ===
  | Basic BasicForeignType
  | Builtin BuiltinForeignType
  deriving stock (Show, Eq)

-- | A basic foreign type as described in the "8.4.2
-- Foreign Types" section of the "Haskell 2010 Language" report:
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
data BasicForeignType =
    -- Prelude
    Char
  | Int
  | Double
  | Float
  | Bool
    -- Data.Int
  | Int8
  | Int16
  | Int32
  | Int64
    -- Data.Word
  | Word
  | Word8
  | Word16
  | Word32
  | Word64
    -- Foreign.Ptr
  | Ptr
  | FunPtr
    -- Foreign.StablePtr
  | StablePtr
  deriving stock (Show, Eq)

-- | A builtin foreign type is a newtype around a basic foreign type that we
-- have explicit support for.
--
-- These builtin foreign types are all non-basic foreign types coming from the
-- "Foreign.C" modules. Most of these types, like 'CInt', are newtypes around
-- basic foreign types, but the specific basic foreign type depends on the
-- platform\/operating system, so for simplicity we decided to include them
-- here.
--
-- These are technically a subset of /marshallable foreign types/ as described
-- in the "8.4.2 Foreign Types" section of the "Haskell 2010 Language" report:
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
data BuiltinForeignType =
    -- Foreign.Ptr
    IntPtr
  | WordPtr
    -- Foreign.C.ConstPtr
  | ConstPtr
    -- Foreign.C.Types
  | CChar
  | CSChar
  | CUChar
  | CShort
  | CUShort
  | CInt
  | CUInt
  | CLong
  | CULong
  | CPtrdiff
  | CSize
  | CWchar
  | CSigAtomic
  | CLLong
  | CULLong
  | CBool
  | CIntPtr
  | CUIntPtr
  | CIntMax
  | CUIntMax
    -- Foreign.C.Types : Numeric types
  | CClock
  | CTime
  | CUSeconds
  | CSUSeconds
    -- Foreign.C.Types : Floating type
  | CFloat
  | CDouble
  deriving stock (Show, Eq)
