-- | Types used by the C standard library external binding specification
module HsBindgen.Runtime.Prelude (
    -- * Primitive types
    -- $PrimitiveTypes
    Foreign.C.CChar(..)
  , Foreign.C.CUChar(..)
  , Foreign.C.CShort(..)
  , Foreign.C.CUShort(..)
  , Foreign.C.CInt(..)
  , Foreign.C.CUInt(..)
  , Foreign.C.CLong(..)
  , Foreign.C.CULong(..)
  , Foreign.C.CLLong(..)
  , Foreign.C.CULLong(..)
  , Foreign.C.CFloat(..)
  , Foreign.C.CDouble(..)
  , Foreign.C.CString

    -- * Boolean types
    -- $BooleanTypes
  , Foreign.C.CBool(..)

    -- * Integral types
    -- $IntegralTypes
  , Data.Int.Int8
  , Data.Int.Int16
  , Data.Int.Int32
  , Data.Int.Int64
  , Data.Word.Word8
  , Data.Word.Word16
  , Data.Word.Word32
  , Data.Word.Word64
  , Foreign.C.CIntMax(..)
  , Foreign.C.CUIntMax(..)
  , Foreign.C.CIntPtr(..)
  , Foreign.C.CUIntPtr(..)

    -- * Floating types
    -- $FloatingTypes
  , LibC.CFenvT
  , LibC.CFexceptT

    -- * Mathematical types
  , LibC.CDivT(..)
  , LibC.CLdivT(..)
  , LibC.CLldivT(..)

    -- * Standard definitions
    -- $StandardDefinitions
  , Foreign.C.CSize(..)
  , Foreign.C.CPtrdiff(..)

    -- * Non-local jump types
    -- $NonLocalJumpTypes
  , Foreign.C.CJmpBuf

    -- * Wide character types
    -- $WideCharacterTypes
  , Foreign.C.CWchar(..)
  , LibC.CWintT(..)
  , LibC.CMbstateT
  , LibC.CWctransT(..)
  , LibC.CWctypeT(..)
  , LibC.CChar16T(..)
  , LibC.CChar32T(..)

    -- * Localization types
    -- $LocalizationTypes

    -- * Time types
    -- $TimeTypes
  , Foreign.C.CTime
  , Foreign.C.CClock
  , LibC.CTm(..)

    -- * File types
    -- $FileTypes
  , Foreign.C.CFile
  , Foreign.C.CFpos

    -- * Signal types
    -- $SignalTypes
  , Foreign.C.CSigAtomic(..)
  ) where

import Data.Int qualified
import Data.Word qualified
import Foreign.C qualified

import HsBindgen.Runtime.LibC qualified as LibC

{-------------------------------------------------------------------------------
  Primitive types
-------------------------------------------------------------------------------}

-- $PrimitiveTypes
--
-- The following types are available in all C standards.  The corresponding
-- Haskell types are defined in @base@ with platform-specific implementations.
--
-- Integral types:
--
-- * @char@ corresponds to Haskell type 'Foreign.C.CChar'.
-- * @unsigned char@ corresponds to Haskell type 'Foreign.C.CUChar'.
-- * @short@ corresponds to Haskell type 'Foreign.C.CShort'.
-- * @unsigned short@ corresponds to Haskell type 'Foreign.C.CUShort'.
-- * @int@ corresponds to Haskell type 'Foreign.C.CInt'.
-- * @unsigned int@ corresponds to Haskell type 'Foreign.C.CUInt'.
-- * @long@ corresponds to Haskell type 'Foreign.C.CLong'.
-- * @unsigned long@ corresponds to Haskell type 'Foreign.C.CULong'.
-- * @long long@ corresponds to Haskell type 'Foreign.C.CLLong'.
-- * @unsigned long long@ corresponds to Haskell type 'Foreign.C.CULLong'.
--
-- Floating types:
--
-- * @float@ corresponds to Haskell type 'Foreign.C.CFloat'.
-- * @double@ corresponds to Haskell type 'Foreign.C.CDouble'.
-- * @long double@ is not yet supported.
--
-- Other types:
--
-- * @char*@ corresponds to Haskell type 'Foreign.C.CString'.

{-------------------------------------------------------------------------------
  Boolean types
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
-- 'Foreign.C.CBool' is defined in @base@ with a platform-specific
-- implementation.  It should be compatible with boolean types across all of the
-- C standards.  Only values @0@ and @1@ may be used even though the
-- representation allows for other values.

{-------------------------------------------------------------------------------
  Integral types
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
--   'Foreign.C.CIntMax', defined in @base@ with a platform-specific
--   implementation, is the corresponding Haskell type.
-- * @uintmax_t@ is the unsigned integral type with the maximum width supported.
--   'Foreign.C.CUIntMax', defined in @base@ with a platform-specific
--   implementation, is the corresponding Haskell type.
-- * @intptr_t@ is a signed integral type capable of holding a value converted
--   from a void pointer and then be converted back to that type with a value
--   that compares equal to the original pointer.  'Foreign.C.CIntPtr', defined
--   in @base@ with a platform-specific implementation, is the corresponding
--   Haskell type.
-- * @uintptr_t@ is an unsigned integral type capable of holding a value
--   converted from a void pointer and then be converted back to that type with
--   a value that compares equal to the original pointer.  'Foreign.C.CUIntPtr',
--   defined in @base@ with a platform-specific implementation, is the
--   corresponding Haskell type.

{-------------------------------------------------------------------------------
  Floating types
-------------------------------------------------------------------------------}

-- $FloatingTypes
--
-- @float_t@, defined in the @math.h@ header file, is not supported yet because
-- it uses @long double@.
--
-- @double_t@, defined in the @math.h@ header file, is not supported yet because
-- it uses @long double@.

{-------------------------------------------------------------------------------
  Standard definitions
-------------------------------------------------------------------------------}

-- $StandardDefinitions
--
-- @size_t@ is an unsigned integral type used to represent the size of objects
-- in memory and dereference elements of an array.  It is defined in the
-- @stddef.h@ header file, and it is made available in many other header files
-- that use it.  'Foreign.C.CSize', defined in @base@ with a platform-specific
-- implementation, is the corresponding Haskell type.
--
-- @ptrdiff_t@ is a type that represents the result of pointer subtraction.  It
-- is defined in the @stddef.h@ header file.  'Foreign.C.CPtrdiff`, defined in
-- @base@ with a platform-specific implementation, is the corresponding Haskell
-- type.
--
-- @max_align_t@, defined in the @stddef.h@ header from C11, is not supported
-- yet because it uses @long double@.

{-------------------------------------------------------------------------------
  Non-local jump types
-------------------------------------------------------------------------------}

-- $NonLocalJumpTypes
--
-- @jmp_buf@ holds information to restore the calling environment.  It is
-- defined in the @setjmp.h@ header file.  'Foreign.C.CJmpBuf', defined in
-- @base@ as an opaque type, may only be used with a 'Foreign.Ptr.Ptr'.

{-------------------------------------------------------------------------------
  Wide character types
-------------------------------------------------------------------------------}

-- $WideCharacterTypes
--
-- @wchar_t@ represents wide characters.  It is available since C95.  It is
-- defined in the @stddef.h@ and @wchar.h@ header files, and it is made
-- available in other header files that use it.  'Foreign.C.CWchar', defined in
-- @base@ with a platform-specific implementation, is the corresponding Haskell
-- type.

{-------------------------------------------------------------------------------
  Localization types
-------------------------------------------------------------------------------}

-- $LocalizationTypes
--
-- @struct lconv@, defined in the @locale.h@ header file, is not supported yet
-- because fields differ across C standards.

{-------------------------------------------------------------------------------
  Time types
-------------------------------------------------------------------------------}

-- $TimeTypes
--
-- @time_t@ represents a point in time.  It is not portable, as libraries may
-- use different time representations.  It is defined in the @time.h@ header
-- file, and it is made available in other header files that use it.
-- 'Foreign.C.CTime', defined in @base@ with a platform-specific implementation,
-- is the corresponding Haskell type.
--
-- @clock_t@ represents clock tick counts, in units of time of a constant but
-- system-specific duration.  It is defined in the @time.h@ header file, and it
-- is made available in other header files that use it.  'Foreign.C.CClock',
-- defined in @base@ with a platform-specific implementation, is the
-- corresponding Haskell type.

{-------------------------------------------------------------------------------
  File types
-------------------------------------------------------------------------------}

-- $FileTypes
--
-- @FILE@ identifies and contains information that controls a stream.  It is
-- defined in the @stdio.h@ header file, and it is made available in other
-- header files that use it.  'Foreign.C.CFile', defined in @base@ as an opaque
-- type, may only be used with a 'Foreign.Ptr.Ptr'.
--
-- @fpos_t@ contains information that specifies a position within a file.  It is
-- defined in the @stdio.h@ header file.  'Foreign.C.CFpos', defined in @base@
-- as an opaque type, may only be used with a 'Foreign.Ptr.Ptr'.

{-------------------------------------------------------------------------------
  Signal types
-------------------------------------------------------------------------------}

-- $SignalTypes
--
-- @sig_atomic_t@ is an integral type that represents an object that can be
-- accessed as an atomic entity even in the presence of asynchronous signals.
-- It is defined in the @signal.h@ header file.  'Foreign.C.CSigAtomic', defined
-- in @base@ with a platform-specific implementation.
