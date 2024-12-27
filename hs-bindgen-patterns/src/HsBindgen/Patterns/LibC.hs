module HsBindgen.Patterns.LibC (
    -- * Primitive Types
    -- $PrimitiveTypes
    -- ** Primitive Integral Types
    CChar(..)
  , CSChar(..)
  , CUChar(..)
  , CShort(..)
  , CUShort(..)
  , CInt(..)
  , CUInt(..)
  , CLong(..)
  , CULong(..)
  , CLLong(..)
  , CULLong(..)
    -- ** Primitive Floating Types
  , CFloat(..)
  , CDouble(..)
    -- ** Primitive Pointer Types
  , Ptr
  , FunPtr
  , CString
    -- * Boolean Types
    -- $BooleanTypes
  , CBool(..)
    -- * Integral Types
    -- $IntegralTypes
  , CInt8(..)
  , CInt16(..)
  , CInt32(..)
  , CInt64(..)
  , CUInt8(..)
  , CUInt16(..)
  , CUInt32(..)
  , CUInt64(..)
  , CIntLeast8(..)
  , CIntLeast16(..)
  , CIntLeast32(..)
  , CIntLeast64(..)
  , CUIntLeast8(..)
  , CUIntLeast16(..)
  , CUIntLeast32(..)
  , CUIntLeast64(..)
  , CIntFast8(..)
  , CIntFast16(..)
  , CIntFast32(..)
  , CIntFast64(..)
  , CUIntFast8(..)
  , CUIntFast16(..)
  , CUIntFast32(..)
  , CUIntFast64(..)
  , CIntMax(..)
  , CUIntMax(..)
  , CIntPtr(..)
  , CUIntPtr(..)
    -- * Floating Types
  , CFexcept(..)
    -- * Mathematical Types
    -- * Standard Definitions
    -- $StandardDefinitions
  , CSize(..)
  , CPtrdiff(..)
    -- * Non-Local Jump Types
    -- $NonLocalJumpTypes
  , CJmpBuf
    -- * Wide Character Types
    -- $WideCharacterTypes
  , CWchar(..)
  , CWint(..)
  , CWctrans(..)
  , CWctype(..)
  , CChar16(..)
  , CChar32(..)
    -- * Localization Types
    -- * Time Types
    -- $TimeTypes
  , CTime(..)
  , CClock(..)
    -- * File Types
    -- $FileTypes
  , CFile
  , CFpos
    -- * Signal Types
    -- $SignalTypes
  , CSigAtomic(..)
    -- * Thread Types
  ) where

import Data.Bits
import Data.Int
import Data.Ix
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- Architecture-dependent definitions are defined in the following /internal/
-- module.  Cabal conditionals are used to select the source corresponding to
-- the host architecture.
import HsBindgen.Patterns.LibC.Arch ()

{-------------------------------------------------------------------------------
  Primitive Types
-------------------------------------------------------------------------------}

-- $PrimitiveTypes
--
-- These "primitive" types are available in all C standards, without import.
--
-- All of these types are defined in @base@ with platform-specific
-- implementations.

-- TODO @long double@

{-------------------------------------------------------------------------------
  Boolean Types
-------------------------------------------------------------------------------}

-- $BooleanTypes
--
-- Boolean data types have integral representations where @0@ represents 'False'
-- and @1@ represents 'True'.
--
-- C standards prior to C99 did not include a boolean type.  C99 defined
-- @_Bool@, which was deprecated in C23.  C23 defines a @bool@ type.  Relevant
-- macros are defined in the @stdbool.h@ header file.
--
-- 'CBool' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  It should be compatible with boolean types across all of the
-- C standards.  Only values @0@ and @1@ should be used even though the
-- representation may represent other values.

{-------------------------------------------------------------------------------
  Integral Types
-------------------------------------------------------------------------------}

-- $IntegralTypes
--
-- 'CIntMax' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @intmax_t@ is the signed integral type with the maximum
-- width supported.  It is defined in the @stdint.h@ header file, and it is also
-- made available by the @inttypes.h@ header file.
--
-- 'CUIntMax' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @uintmax_t@ is the unsigned integral type with the maximum
-- width supported.  It is defined in the @stdint.h@ header file, and it is also
-- made available by the @inttypes.h@ header file.
--
-- 'CIntPtr' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @intptr_t@ is a signed integral type capable of holding a
-- value converted from a void pointer and then be converted back to that type
-- with a value that compares equal to the original pointer.  It is defined in
-- the @stdint.h@ header file, and it is also made available by the @inttypes.h@
-- header file.
--
-- 'CUIntPtr' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @uintptr_t@ is an unsigned integral type capable of holding
-- a value converted from a void pointer and then be converted back to that type
-- with a value that compares equal to the original pointer.  It is defined in
-- the @stdint.h@ header file, and it is also made available by the @inttypes.h@
-- header file.

-- | C 'int8_t' type
--
-- @int8_t@ is a signed integral type with exactly 8 bits.  It is available
-- since C99.  It is defined in the @stdint.h@ header file, and it is also made
-- available by the @inttypes.h@ header file.
newtype CInt8 = CInt8 Int8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int16_t' type
--
-- @int16_t@ is a signed integral type with exactly 16 bits.  It is available
-- since C99.  It is defined in the @stdint.h@ header file, and it is also made
-- available by the @inttypes.h@ header file.
newtype CInt16 = CInt16 Int16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int32_t' type
--
-- @int32_t@ is a signed integral type with exactly 32 bits.  It is available
-- since C99.  It is defined in the @stdint.h@ header file, and it is also made
-- available by the @inttypes.h@ header file.
newtype CInt32 = CInt32 Int32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int64_t' type
--
-- @int64_t@ is a signed integral type with exactly 64 bits.  It is available
-- since C99.  It is defined in the @stdint.h@ header file, and it is also made
-- available by the @inttypes.h@ header file.
newtype CInt64 = CInt64 Int64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint8_t' type
--
-- @uint8_t@ is an unsigned integral type with exactly 8 bits.  It is available
-- since C99.  It is defined in the @stdint.h@ header file, and it is also made
-- available by the @inttypes.h@ header file.
newtype CUInt8 = CUInt8 Word8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint16_t' type
--
-- @uint16_t@ is an unsigned integral type with exactly 16 bits.  It is
-- available since C99.  It is defined in the @stdint.h@ header file, and it is
-- also made available by the @inttypes.h@ header file.
newtype CUInt16 = CUInt16 Word16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint32_t' type
--
-- @uint32_t@ is an unsigned integral type with exactly 32 bits.  It is
-- available since C99.  It is defined in the @stdint.h@ header file, and it is
-- also made available by the @inttypes.h@ header file.
newtype CUInt32 = CUInt32 Word32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint64_t' type
--
-- @uint64_t@ is an unsigned integral type with exactly 64 bits.  It is
-- available since C99.  It is defined in the @stdint.h@ header file, and it is
-- also made available by the @inttypes.h@ header file.
newtype CUInt64 = CUInt64 Word64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_least8_t' type
--
-- @int_least8_t@ is a signed integral type with at least 8 bits, such that no
-- other signed integral type exists with a smaller size and at least 8 bits.
-- It is available since C99.  It is defined in the @stdint.h@ header file, and
-- it is also made available by the @inttypes.h@ header file.
newtype CIntLeast8 = CIntLeast8 CInt8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_least16_t' type
--
-- @int_least16_t@ is a signed integral type with at least 16 bits, such that no
-- other signed integral type exists with a smaller size and at least 16 bits.
-- It is available since C99.  It is defined in the @stdint.h@ header file, and
-- it is also made available by the @inttypes.h@ header file.
newtype CIntLeast16 = CIntLeast16 CInt16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_least32_t' type
--
-- @int_least32_t@ is a signed integral type with at least 32 bits, such that no
-- other signed integral type exists with a smaller size and at least 32 bits.
-- It is available since C99.  It is defined in the @stdint.h@ header file, and
-- it is also made available by the @inttypes.h@ header file.
newtype CIntLeast32 = CIntLeast32 CInt32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_least64_t' type
--
-- @int_least64_t@ is a signed integral type with at least 64 bits, such that no
-- other signed integral type exists with a smaller size and at least 64 bits.
-- It is available since C99.  It is defined in the @stdint.h@ header file, and
-- it is also made available by the @inttypes.h@ header file.
newtype CIntLeast64 = CIntLeast64 CInt64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_least8_t' type
--
-- @uint_least8_t@ is an unsigned integral type with at least 8 bits, such that
-- no other unsigned integral type exists with a smaller size and at least 8
-- bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntLeast8 = CUIntLeast8 CUInt8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_least16_t' type
--
-- @uint_least16_t@ is an unsigned integral type with at least 16 bits, such
-- that no other unsigned integral type exists with a smaller size and at least
-- 16 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntLeast16 = CUIntLeast16 CUInt16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_least32_t' type
--
-- @uint_least32_t@ is an unsigned integral type with at least 32 bits, such
-- that no other unsigned integral type exists with a smaller size and at least
-- 32 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntLeast32 = CUIntLeast32 CUInt32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_least64_t' type
--
-- @uint_least64_t@ is an unsigned integral type with at least 64 bits, such
-- that no other unsigned integral type exists with a smaller size and at least
-- 64 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntLeast64 = CUIntLeast64 CUInt64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_fast8_t' type
--
-- @int_fast8_t@ is a signed integral type with at least 8 bits, such that it is
-- at least as fast as any other signed integral type that has at least 8 bits.
-- It is available since C99.  It is defined in the @stdint.h@ header file, and
-- it is also made available by the @inttypes.h@ header file.
newtype CIntFast8 = CIntFast8 CInt8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_fast16_t' type
--
-- @int_fast16_t@ is a signed integral type with at least 16 bits, such that it
-- is at least as fast as any other signed integral type that has at least
-- 16 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CIntFast16 = CIntFast16 CInt16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_fast32_t' type
--
-- @int_fast32_t@ is a signed integral type with at least 32 bits, such that it
-- is at least as fast as any other signed integral type that has at least
-- 32 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CIntFast32 = CIntFast32 CInt32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'int_fast64_t' type
--
-- @int_fast64_t@ is a signed integral type with at least 64 bits, such that it
-- is at least as fast as any other signed integral type that has at least
-- 64 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CIntFast64 = CIntFast64 CInt64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_fast8_t' type
--
-- @uint_fast8_t@ is an unsigned integral type with at least 8 bits, such that
-- it is at least as fast as any other unsigned integral type that has at least
-- 8 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntFast8 = CUIntFast8 CUInt8
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_fast16_t' type
--
-- @uint_fast16_t@ is an unsigned integral type with at least 16 bits, such that
-- it is at least as fast as any other unsigned integral type that has at least
-- 16 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntFast16 = CUIntFast16 CUInt16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_fast32_t' type
--
-- @uint_fast32_t@ is an unsigned integral type with at least 32 bits, such that
-- it is at least as fast as any other unsigned integral type that has at least
-- 32 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntFast32 = CUIntFast32 CUInt32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C 'uint_fast64_t' type
--
-- @uint_fast64_t@ is an unsigned integral type with at least 64 bits, such that
-- it is at least as fast as any other unsigned integral type that has at least
-- 64 bits.  It is available since C99.  It is defined in the @stdint.h@ header
-- file, and it is also made available by the @inttypes.h@ header file.
newtype CUIntFast64 = CUIntFast64 CUInt64
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

{-------------------------------------------------------------------------------
  Floating Types
-------------------------------------------------------------------------------}

-- TODO @float_t@ (arch, uses long double, name, math.h)

-- TODO @double_t@ (arch, uses long double, name, math.h)

-- TODO @fenv_t@ (arch, name, C99, fenv.h)

-- | C @fexcept_t@ type
--
-- @fexcept_t@ represents the state of all floating-point status flags
-- collectively, including the active floating-point exceptions along with any
-- additional information the implementation associates with their status.  It
-- is available since C99.  It is defined in the @fenv.h@ header file.
newtype CFexcept = CFexcept CShort
  deriving newtype (Eq, Ord, Show, Storable)

{-------------------------------------------------------------------------------
  Mathematical Types
-------------------------------------------------------------------------------}

-- TODO @div_t@ (name, stdlib.h)

-- TODO @ldiv_t@ (name, stdlib.h)

-- TODO @lldiv_t@ (name, stdlib.h)

-- TODO @imaxdiv_t@ (name, C99, inttypes.h)

{-------------------------------------------------------------------------------
  Standard Definitions
-------------------------------------------------------------------------------}

-- $StandardDefinitions
--
-- 'CSize' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @size_t@ is an unsigned integral type used to represent the
-- size of objects in memory and dereference elements of an array.  It is
-- defined in the @stddef.h@ header file, and it is made available in many other
-- header files that use it.
--
-- 'CPtrdiff` is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @ptrdiff_t@ is a type that represents the result of pointer
-- subtraction.  It is defined in the @stddef.h@ header file.

-- TODO @max_align_t@ (uses long double, C11, stddef.h)

{-------------------------------------------------------------------------------
  Non-Local Jump Types
-------------------------------------------------------------------------------}

-- $NonLocalJumpTypes
--
-- 'CJmpBuf' is defined in "Foreign.C.Types" as an opaque type, so it may only
-- be used with a 'Ptr'.  @jmp_buf@ holds information to restore the calling
-- environment.  It is defined in the @setjmp.h@ header file.

{-------------------------------------------------------------------------------
  Wide Character Types
-------------------------------------------------------------------------------}

-- $WideCharacterTypes
--
-- 'CWchar' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @wchar_t@ represents wide characters.  It is available since
-- C95.  It is defined in the @stddef.h@ and @wchar.h@ header files, and it is
-- made available in other header files that use it.

-- | C @wint_t@ type
--
-- @wint_t@ represents wide integers.  It is available since C95.  It is defined
-- in the @wchar.h@ and @wctype.h@ header files.
newtype CWint = CWint CUInt
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- TODO @mbstate_t@ (opaque, C95, wchar.h uchar.h)

-- | C @wctrans_t@ type
--
-- @wctrans_t@ is a scalar type that can hold values which represent
-- locale-specific character transformations.  It is available since C95.  It is
-- defined in the @wctype.h@ header file.
newtype CWctrans = CWctrans (Ptr CInt)
  deriving newtype (Eq, Show, Storable)

-- | C @wctype_t@ type
--
-- @wctype_t@ is a scalar type that can hold values which represent
-- locale-specific character classification categories.  It is available since
-- C95.  It is defined in the @wctype.h@ and @wchar.h@ header files.
newtype CWctype = CWctype CULong
  deriving newtype (Eq, Show, Storable)

-- | C @char16_t@ type
--
-- @char16_t@ represents a 16-bit Unicode character.  It is available since C11.
-- It is defined in the @uchar.h@ header file.
newtype CChar16 = CChar16 CUIntLeast16
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

-- | C @char32_t@ type
--
-- @char32_t@ represents a 32-bit Unicode character.  It is available since C11.
-- It is defined in the @uchar.h@ header file.
newtype CChar32 = CChar32 CUIntLeast32
  deriving newtype (
      Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , Integral
    , Ix
    , Num
    , Ord
    , Read
    , Real
    , Show
    , Storable
    )

{-------------------------------------------------------------------------------
  Localization Types
-------------------------------------------------------------------------------}

-- TODO @struct lconv@ (locale.h)

{-------------------------------------------------------------------------------
  Time Types
-------------------------------------------------------------------------------}

-- $TimeTypes
--
-- 'CTime' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @time_t@ represents a point in time.  It is not portable, as
-- libraries may use different time representations.  It is defined in the
-- @time.h@ header file, and it is made available in other header files that use
-- it.
--
-- 'CClock' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @clock_t@ represents clock tick counts, in units of time of
-- a constant but system-specific duration.  It is defined in the @time.h@
-- header file, and it is made available in other header files that use it.

-- TODO @struct tm@ (name, time.h)

{-------------------------------------------------------------------------------
  File Types
-------------------------------------------------------------------------------}

-- $FileTypes
--
-- 'CFile' is defined in "Foreign.C.Types" as an opaque type, so it may only
-- be used with a 'Ptr'.  @FILE@ identifies and contains information that
-- controls a stream.  It is defined in the @stdio.h@ header file, and it is
-- made available in other header files that use it.
--
-- 'CFpos' is defined in "Foreign.C.Types" as an opaque type, so it may only
-- be used with a 'Ptr'.  @fpos_t@ contains information that specifies a
-- position within a file.  It is defined in the @stdio.h@ header file.

{-------------------------------------------------------------------------------
  Signal Types
-------------------------------------------------------------------------------}

-- $SignalTypes
--
-- 'CSigAtomic' is defined in "Foreign.C.Types" with a platform-specific
-- implementation.  @sig_atomic_t@ is an integral type that represents an object
-- that can be accessed as an atomic entity even in the presence of asynchronous
-- signals.  It is defined in the @signal.h@ header file.

{-------------------------------------------------------------------------------
  Thread Types
-------------------------------------------------------------------------------}

-- TODO threads.h (C11)
