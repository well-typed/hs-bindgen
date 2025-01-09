module HsBindgen.Patterns.LibC (
    -- * Primitive Types
    -- $PrimitiveTypes
    -- * Boolean Types
    -- $BooleanTypes
    -- * Integral Types
    -- $IntegralTypes
    -- * Floating Types
    CFexceptT(..)
    -- * Mathematical Types
    -- * Standard Definitions
    -- $StandardDefinitions
    -- * Non-Local Jump Types
    -- $NonLocalJumpTypes
    -- * Wide Character Types
    -- $WideCharacterTypes
  , CWintT(..)
  , CWctransT(..)
  , CWctypeT(..)
  , CChar16T(..)
  , CChar32T(..)
    -- * Localization Types
    -- * Time Types
    -- $TimeTypes
    -- * File Types
    -- $FileTypes
    -- * Signal Types
    -- $SignalTypes
    -- * Thread Types
  ) where

import Data.Bits (Bits, FiniteBits)
import Data.Ix (Ix)
import Data.Word (Word16, Word32)
import Foreign.C.Types qualified as C
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)

-- Architecture-dependent definitions are defined in the following /internal/
-- module.  Cabal conditionals are used to select the source corresponding to
-- the host architecture.
import HsBindgen.Patterns.LibC.Arch ()

{-------------------------------------------------------------------------------
  Primitive Types
-------------------------------------------------------------------------------}

-- $PrimitiveTypes
--
-- The following types are available in all C standards.  They corresponding
-- Haskell types are defined in @base@ with platform-specific implementations.
--
-- Integral types:
--
-- * @char@ corresponds to Haskell type 'C.CChar'.
-- * @unsigned char@ corresponds to Haskell type 'C.CUChar'.
-- * @short@ corresponds to Haskell type 'C.CShort'.
-- * @unsigned short@ corresponds to Haskell type 'C.CUShort'.
-- * @int@ corresponds to Haskell type 'C.CInt'.
-- * @unsigned int@ corresponds to Haskell type 'C.CUInt'.
-- * @long@ corresponds to Haskell type 'C.CLong'.
-- * @unsigned long@ corresponds to Haskell type 'C.CULong'.
-- * @long long@ corresponds to Haskell type 'C.CLLong'.
-- * @unsigned long long@ corresponds to Haskell type 'C.CULLong'.
--
-- Floating types:
--
-- * @float@ corresponds to Haskell type 'C.CFloat'.
-- * @double@ corresponds to Haskell type 'C.CDouble'.
-- * @long double@ is not yet supported.
--
-- Other types:
--
-- * @char*@ corresponds to Haskell type 'Foreign.C.String.CString'.

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
-- 'C.CBool' is defined in @base@ with a platform-specific implementation.  It
-- should be compatible with boolean types across all of the C standards.  Only
-- values @0@ and @1@ may be used even though the representation allows for
-- other values.

{-------------------------------------------------------------------------------
  Integral Types
-------------------------------------------------------------------------------}

-- $IntegralTypes
--
-- The following C types are available since C99 and are provided by the
-- @stdint.h@ and @inttypes.h@ header files.
--
-- * @int8_t@ is a signed integral type with exactly 8 bits.  'Data.Int.Int8' is
--   the corresponding Haskell type.
-- * @int16_t@ is a signed integral type with exactly 16 bits.  'Data.Int.Int16'
--   is the corresponding Haskell type.
-- * @int32_t@ is a signed integral type with exactly 32 bits.  'Data.Int.Int32'
--   is the corresponding Haskell type.
-- * @int64_t@ is a signed integral type with exactly 64 bits.  'Data.Int.Int64'
--   is the corresponding Haskell type.
-- * @uint8_t@ is an unsigned integral type with exactly 8 bits.
--   'Data.Word.Word8' is the corresponding Haskell type.
-- * @uint16_t@ is an unsigned integral type with exactly 16 bits.
--   'Data.Word.Word16' is the corresponding Haskell type.
-- * @uint32_t@ is an unsigned integral type with exactly 32 bits.
--   'Data.Word.Word32' is the corresponding Haskell type.
-- * @uint64_t@ is an unsigned integral type with exactly 64 bits.
--   'Data.Word.Word64' is the corresponding Haskell type.
-- * @int_least8_t@ is a signed integral type with at least 8 bits, such that no
--   other signed integral type exists with a smaller size and at least 8 bits.
--   'Data.Int.Int8' is the corresponding Haskell type.
-- * @int_least16_t@ is a signed integral type with at least 16 bits, such that
--   no other signed integral type exists with a smaller size and at least 16
--   bits.  'Data.Int.Int16' is the corresponding Haskell type.
-- * @int_least32_t@ is a signed integral type with at least 32 bits, such that
--   no other signed integral type exists with a smaller size and at least 32
--   bits.  'Data.Int.Int32' is the corresponding Haskell type.
-- * @int_least64_t@ is a signed integral type with at least 64 bits, such that
--   no other signed integral type exists with a smaller size and at least 64
--   bits.  'Data.Int.Int64' is the corresponding Haskell type.
-- * @uint_least8_t@ is an unsigned integral type with at least 8 bits, such
--   that no other unsigned integral type exists with a smaller size and at
--   least 8 bits.  'Data.Word.Word8' is the corresponding Haskell type.
-- * @uint_least16_t@ is an unsigned integral type with at least 16 bits, such
--   that no other unsigned integral type exists with a smaller size and at
--   least 16 bits.  'Data.Word.Word16' is the corresponding Haskell type.
-- * @uint_least32_t@ is an unsigned integral type with at least 32 bits, such
--   that no other unsigned integral type exists with a smaller size and at
--   least 32 bits.  'Data.Word.Word32' is the corresponding Haskell type.
-- * @uint_least64_t@ is an unsigned integral type with at least 64 bits, such
--   that no other unsigned integral type exists with a smaller size and at
--   least 64 bits.  'Data.Word.Word64' is the corresponding Haskell type.
-- * @int_fast8_t@ is a signed integral type with at least 8 bits, such that it
--   is at least as fast as any other signed integral type that has at least 8
--   bits.  'Data.Int.Int8' is the corresponding Haskell type.
-- * @int_fast16_t@ is a signed integral type with at least 16 bits, such that
--   it is at least as fast as any other signed integral type that has at least
--   16 bits.  'Data.Int.Int16' is the corresponding Haskell type.
-- * @int_fast32_t@ is a signed integral type with at least 32 bits, such that
--   it is at least as fast as any other signed integral type that has at least
--   32 bits.  'Data.Int.Int32' is the corresponding Haskell type.
-- * @int_fast64_t@ is a signed integral type with at least 64 bits, such that
--   it is at least as fast as any other signed integral type that has at least
--   64 bits.  'Data.Int.Int64' is the corresponding Haskell type.
-- * @uint_fast8_t@ is an unsigned integral type with at least 8 bits, such that
--   it is at least as fast as any other unsigned integral type that has at
--   least 8 bits.  'Data.Word.Word8' is the corresponding Haskell type.
-- * @uint_fast16_t@ is an unsigned integral type with at least 16 bits, such
--   that it is at least as fast as any other unsigned integral type that has at
--   least 16 bits.  'Data.Word.Word16' is the corresponding Haskell type.
-- * @uint_fast32_t@ is an unsigned integral type with at least 32 bits, such
--   that it is at least as fast as any other unsigned integral type that has at
--   least 32 bits.  'Data.Word.Word32' is the corresponding Haskell type.
-- * @uint_fast64_t@ is an unsigned integral type with at least 64 bits, such
--   that it is at least as fast as any other unsigned integral type that has at
--   least 64 bits.  'Data.Word.Word64' is the corresponding Haskell type.
-- * @intmax_t@ is the signed integral type with the maximum width supported.
--   'C.CIntMax', defined in @base@ with a platform-specific implementation, is
--   the corresponding Haskell type.
-- * @uintmax_t@ is the unsigned integral type with the maximum width supported.
--   'C.CUIntMax', defined in @base@ with a platform-specific implementation, is
--   the corresponding Haskell type.
-- * @intptr_t@ is a signed integral type capable of holding a value converted
--   from a void pointer and then be converted back to that type with a value
--   that compares equal to the original pointer.  'C.CIntPtr', defined in
--   @base@ with a platform-specific implementation, is the corresponding
--   Haskell type.
-- * @uintptr_t@ is an unsigned integral type capable of holding a value
--   converted from a void pointer and then be converted back to that type with
--   a value that compares equal to the original pointer.  'C.CUIntPtr', defined
--   in @base@ with a platform-specific implementation, is the corresponding
--   Haskell type.

{-------------------------------------------------------------------------------
  Floating Types
-------------------------------------------------------------------------------}

-- TODO CFloatT @float_t@ (arch, uses long double, math.h)

-- TODO CDoubleT @double_t@ (arch, uses long double, math.h)

-- TODO CFenvT @fenv_t@ (arch, C99, fenv.h)

-- | C @fexcept_t@ type
--
-- @fexcept_t@ represents the state of all floating-point status flags
-- collectively, including the active floating-point exceptions along with any
-- additional information the implementation associates with their status.  It
-- is available since C99.  It is defined in the @fenv.h@ header file.
newtype CFexceptT = CFexceptT C.CShort
  deriving newtype (Eq, Ord, Show, Storable)

{-------------------------------------------------------------------------------
  Mathematical Types
-------------------------------------------------------------------------------}

-- TODO CDivT @div_t@ (stdlib.h)

-- TODO CLdivT @ldiv_t@ (stdlib.h)

-- TODO CLldivT @lldiv_t@ (stdlib.h)

-- TODO CImaxdivT @imaxdiv_t@ (C99, inttypes.h)

{-------------------------------------------------------------------------------
  Standard Definitions
-------------------------------------------------------------------------------}

-- $StandardDefinitions
--
-- @size_t@ is an unsigned integral type used to represent the size of objects
-- in memory and dereference elements of an array.  It is defined in the
-- @stddef.h@ header file, and it is made available in many other header files
-- that use it.  'C.CSize', defined in @base@ with a platform-specific
-- implementation, is the corresponding Haskell type.
--
-- @ptrdiff_t@ is a type that represents the result of pointer subtraction.  It
-- is defined in the @stddef.h@ header file.  'C.CPtrdiff`, defined in @base@
-- with a platform-specific implementation, is the corresponding Haskell type.

-- TODO CMaxAlignT @max_align_t@ (uses long double, C11, stddef.h)

{-------------------------------------------------------------------------------
  Non-Local Jump Types
-------------------------------------------------------------------------------}

-- $NonLocalJumpTypes
--
-- @jmp_buf@ holds information to restore the calling environment.  It is
-- defined in the @setjmp.h@ header file.  'C.CJmpBuf', defined in @base@ as an
-- opaque type, may only be used with a 'Ptr'.

{-------------------------------------------------------------------------------
  Wide Character Types
-------------------------------------------------------------------------------}

-- $WideCharacterTypes
--
-- @wchar_t@ represents wide characters.  It is available since C95.  It is
-- defined in the @stddef.h@ and @wchar.h@ header files, and it is made
-- available in other header files that use it.  'C.CWchar', defined in @base@
-- with a platform-specific implementation, is the corresponding Haskell type.

-- | C @wint_t@ type
--
-- @wint_t@ represents wide integers.  It is available since C95.  It is defined
-- in the @wchar.h@ and @wctype.h@ header files.
newtype CWintT = CWintT C.CUInt
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

-- TODO CMbstateT @mbstate_t@ (opaque, C95, wchar.h uchar.h)

-- | C @wctrans_t@ type
--
-- @wctrans_t@ is a scalar type that can hold values which represent
-- locale-specific character transformations.  It is available since C95.  It is
-- defined in the @wctype.h@ header file.
newtype CWctransT = CWctransT (Ptr C.CInt)
  deriving newtype (Eq, Show, Storable)

-- | C @wctype_t@ type
--
-- @wctype_t@ is a scalar type that can hold values which represent
-- locale-specific character classification categories.  It is available since
-- C95.  It is defined in the @wctype.h@ and @wchar.h@ header files.
newtype CWctypeT = CWctypeT C.CULong
  deriving newtype (Eq, Show, Storable)

-- | C @char16_t@ type
--
-- @char16_t@ represents a 16-bit Unicode character.  It is available since C11.
-- It is defined in the @uchar.h@ header file.
newtype CChar16T = CChar16T Word16
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
newtype CChar32T = CChar32T Word32
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

-- TODO CLconv @struct lconv@ (locale.h)

{-------------------------------------------------------------------------------
  Time Types
-------------------------------------------------------------------------------}

-- $TimeTypes
--
-- @time_t@ represents a point in time.  It is not portable, as libraries may
-- use different time representations.  It is defined in the @time.h@ header
-- file, and it is made available in other header files that use it.  'C.CTime',
-- defined in @base@ with a platform-specific implementation, is the
-- corresponding Haskell type.
--
-- @clock_t@ represents clock tick counts, in units of time of a constant but
-- system-specific duration.  It is defined in the @time.h@ header file, and it
-- is made available in other header files that use it.  'C.CClock', defined in
-- @base@ with a platform-specific implementation, is the corresponding Haskell
-- type.

-- TODO CTm @struct tm@ (name, time.h)

{-------------------------------------------------------------------------------
  File Types
-------------------------------------------------------------------------------}

-- $FileTypes
--
-- @FILE@ identifies and contains information that controls a stream.  It is
-- defined in the @stdio.h@ header file, and it is made available in other
-- header files that use it.  'C.CFile', defined in @base@ as an opaque type,
-- may only be used with a 'Ptr'.
--
-- @fpos_t@ contains information that specifies a position within a file.  It is
-- defined in the @stdio.h@ header file.  'C.CFpos', defined in @base@ as an
-- opaque type, may only be used with a 'Ptr'.

{-------------------------------------------------------------------------------
  Signal Types
-------------------------------------------------------------------------------}

-- $SignalTypes
--
-- @sig_atomic_t@ is an integral type that represents an object that can be
-- accessed as an atomic entity even in the presence of asynchronous signals.
-- It is defined in the @signal.h@ header file.  'C.CSigAtomic', defined in
-- @base@ with a platform-specific implementation.

{-------------------------------------------------------------------------------
  Thread Types
-------------------------------------------------------------------------------}

-- TODO threads.h (C11)
