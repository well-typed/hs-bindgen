-- | Datatypes for the 'HsBindgen.Runtime.HasFFIType.HasFFIType' class.
--
-- These datatypes reify all possible values of the
-- 'HsBindgen.Runtime.HasFFIType.FFIType' type. These datatypes are for internal
-- use in the 'HsBindgen.Runtime.HasFFIType.HasFFIType' class, and they are
-- exported for the @hs-bindgen@ package to use. You probably won't need to use
-- these directly, but you might find the datatypes useful as documentation for
-- the 'HsBindgen.Runtime.HasFFIType.HasFFIType' class.
module HsBindgen.Runtime.FFIType (
    FFIType (..)
  , BasicFFIType (..)
  ) where

-- | A foreign type without newtypes
--
-- This datatype does not restrict itself to valid foreign types. For example,
-- @IO Unit `FunArrow` IO Unit@ is not a valid foreign type, but we can
-- represent it nonetheless. See the discussion on the
-- 'HsBindgen.Runtime.HasFFIType.HasFFIType' class for why this
-- is okay.
--
-- Also, see the "8.4.2 Foreign Types" section of the report for more
-- information about foreign types:
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
data FFIType =
    -- === Foreign types ===
    FunArrow FFIType FFIType

    -- === Marshallable foreign result types ===
  | Unit
  | IO FFIType

    -- === Marshallable foreign types ===
  | Basic BasicFFIType
  deriving stock (Show, Eq, Ord)

-- | A basic foreign type as described in the "8.4.2
-- Foreign Types" section of the "Haskell 2010 Language" report:
--
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch8.html#x15-1560008.4.2>
data BasicFFIType =
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
  deriving stock (Show, Eq, Ord, Enum, Bounded)
