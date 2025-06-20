-- | C standard library types that are not in @base@
module HsBindgen.Runtime.LibC (
    -- * Floating Types
    CFenvT
  , CFexceptT

    -- * Mathematical Types
  , CDivT(..)
  , CLdivT(..)
  , CLldivT(..)

    -- * Wide Character Types
  , CWintT(..)
  , CMbstateT
  , CWctransT(..)
  , CWctypeT(..)
  , CChar16T(..)
  , CChar32T(..)

    -- * Time Types
  , CTm(..)
  ) where

import Data.Bits (Bits, FiniteBits)
import Data.Ix (Ix)
import Data.Word (Word16, Word32)
import Foreign.C.Types qualified as C
import Foreign.Ptr (Ptr)
import Foreign.Storable

import HsBindgen.Runtime.Marshal

#include <inttypes.h>
#include <locale.h>
#include <stdlib.h>
#include <time.h>

{-------------------------------------------------------------------------------
  Floating Types
-------------------------------------------------------------------------------}

-- TODO CFloatT @float_t@ (arch, uses long double, math.h)

-- TODO CDoubleT @double_t@ (arch, uses long double, math.h)

-- | C @fenv_t@ type
--
-- @fenv_t@ represents the entire floating-point environment.  It is
-- implementation-specific, so this representation is opaque and may only be
-- used with a 'Ptr'.  It is available since C99.  It is defined in the @fenv.h@
-- header file.
data CFenvT

-- | C @fexcept_t@ type
--
-- @fexcept_t@ represents the floating-point status flags collectively,
-- including any status the implementation associates with the flags.  It is
-- implementation-specific, so this representation is opaque and may only be
-- used with a 'Ptr'.  It is available since C99.  It is defined in the @fenv.h@
-- header file.
data CFexceptT

{-------------------------------------------------------------------------------
  Mathematical Types
-------------------------------------------------------------------------------}

-- | C @div_t@ structure
--
-- @div_t@ represents the result of integral division performed by function
-- @div@.  It is defined in the @stdlib.h@ header file.
data CDivT = CDivT {
      cDivT_quot :: C.CInt -- ^ Quotient
    , cDivT_rem  :: C.CInt -- ^ Remainder
    }
  deriving (Eq, Ord, Show)

instance ReadRaw CDivT where
  readRaw ptr = do
    cDivT_quot <- (#peek div_t, quot) ptr
    cDivT_rem  <- (#peek div_t, rem)  ptr
    return CDivT{..}

-- | C @ldiv_t@ structure
--
-- @ldiv_t@ represents the result of integral division performed by function
-- @ldiv@.  It is defined in the @stdlib.h@ header file.
data CLdivT = CLdivT {
      cLdivT_quot :: C.CLong -- ^ Quotient
    , cLdivT_rem  :: C.CLong -- ^ Remainder
    }
  deriving (Eq, Ord, Show)

instance ReadRaw CLdivT where
  readRaw ptr = do
    cLdivT_quot <- (#peek ldiv_t, quot) ptr
    cLdivT_rem  <- (#peek ldiv_t, rem)  ptr
    return CLdivT{..}

-- | C @lldiv_t@ structure
--
-- @lldiv_t@ represents the result of integral division performed by function
-- @lldiv@.  It is defined in the @stdlib.h@ header file.
data CLldivT = CLldivT {
      cLldivT_quot :: C.CLLong -- ^ Quotient
    , cLldivT_rem  :: C.CLLong -- ^ Remainder
    }
  deriving (Eq, Ord, Show)

instance ReadRaw CLldivT where
  readRaw ptr = do
    cLldivT_quot <- (#peek lldiv_t, quot) ptr
    cLldivT_rem  <- (#peek lldiv_t, rem)  ptr
    return CLldivT{..}

-- | C @imaxdiv_t@ structure
--
-- @imaxdiv_t@ represents the result of integral division performed by function
-- @imaxdiv@.  It is available since C99.  It is defined in the @inttypes.h@
-- header file.
data CImaxdivT = CImaxdivT {
      cImaxdivT_quot :: C.CIntMax -- ^ Quotient
    , cImaxdivT_rem  :: C.CIntMax -- ^ Remainder
    }
  deriving (Eq, Ord, Show)

instance ReadRaw CImaxdivT where
  readRaw ptr = do
    cImaxdivT_quot <- (#peek imaxdiv_t, quot) ptr
    cImaxdivT_rem  <- (#peek imaxdiv_t, rem)  ptr
    return CImaxdivT{..}

{-------------------------------------------------------------------------------
  Standard Definitions
-------------------------------------------------------------------------------}

-- TODO CMaxAlignT @max_align_t@ (uses long double, C11, stddef.h)

{-------------------------------------------------------------------------------
  Wide Character Types
-------------------------------------------------------------------------------}

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
    , ReadRaw
    , Real
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

-- | C @mbstate_t@ type
--
-- @mbstate_t@ is a complete object type other than an array type that can hold
-- the conversion state information necessary to convert between sequences of
-- multibyte characters and wide characters.  It is implementation-specific, so
-- this representation is opaque and may only be used with a 'Ptr'.  It is
-- available since C95.  It is defined in the @wchar.h@ and @uchar.h@ header
-- files.
data CMbstateT

-- | C @wctrans_t@ type
--
-- @wctrans_t@ is a scalar type that can hold values which represent
-- locale-specific character transformations.  It is available since C95.  It is
-- defined in the @wctype.h@ header file.
newtype CWctransT = CWctransT (Ptr C.CInt)
  deriving newtype (Eq, ReadRaw, Show, StaticSize, Storable, WriteRaw)

-- | C @wctype_t@ type
--
-- @wctype_t@ is a scalar type that can hold values which represent
-- locale-specific character classification categories.  It is available since
-- C95.  It is defined in the @wctype.h@ and @wchar.h@ header files.
newtype CWctypeT = CWctypeT C.CULong
  deriving newtype (Eq, ReadRaw, Show, StaticSize, Storable, WriteRaw)

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
    , ReadRaw
    , Real
    , Show
    , StaticSize
    , Storable
    , WriteRaw
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
    , ReadRaw
    , Real
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

{-------------------------------------------------------------------------------
  Localization Types
-------------------------------------------------------------------------------}

-- TODO CLconv @struct lconv@ (fields added in C99, locale.h)

{-------------------------------------------------------------------------------
  Time Types
-------------------------------------------------------------------------------}

-- | C @struct tm@ type
--
-- @struct tm@ holds the components of a calendar time, called the
-- /broken-down time/.  Note that only the fields defined in the standard are
-- represented here.  It is defined in the @time.h@ header file, and it is made
-- available in other header files that use it.
data CTm = CTm {
      cTm_sec   :: C.CInt -- ^ Seconds after the minute (@[0, 60]@)
    , cTm_min   :: C.CInt -- ^ Minutes after the hour (@[0, 59]@)
    , cTm_hour  :: C.CInt -- ^ Hours since midnight (@[0, 23]@)
    , cTm_mday  :: C.CInt -- ^ Day of the month (@[1, 31]@)
    , cTm_mon   :: C.CInt -- ^ Months since January (@[0, 11]@)
    , cTm_year  :: C.CInt -- ^ Years since 1900
    , cTm_wday  :: C.CInt -- ^ Days since Sunday (@[0, 6]@)
    , cTm_yday  :: C.CInt -- ^ Days since January 1 (@[0, 365]@)
    , cTm_isdst :: C.CInt -- ^ Daylight Saving Time flag
    }
  deriving (Eq, Show)
  deriving Storable via EquivStorable CTm

instance StaticSize CTm where
  staticSizeOf    _ = #size      struct tm
  staticAlignment _ = #alignment struct tm

instance ReadRaw CTm where
  readRaw ptr = do
    cTm_sec   <- (#peek struct tm, tm_sec)   ptr
    cTm_min   <- (#peek struct tm, tm_min)   ptr
    cTm_hour  <- (#peek struct tm, tm_hour)  ptr
    cTm_mday  <- (#peek struct tm, tm_mday)  ptr
    cTm_mon   <- (#peek struct tm, tm_mon)   ptr
    cTm_year  <- (#peek struct tm, tm_year)  ptr
    cTm_wday  <- (#peek struct tm, tm_wday)  ptr
    cTm_yday  <- (#peek struct tm, tm_yday)  ptr
    cTm_isdst <- (#peek struct tm, tm_isdst) ptr
    return CTm{..}

instance WriteRaw CTm where
  writeRaw ptr CTm{..} = do
    (#poke struct tm, tm_sec)   ptr cTm_sec
    (#poke struct tm, tm_min)   ptr cTm_min
    (#poke struct tm, tm_hour)  ptr cTm_hour
    (#poke struct tm, tm_mday)  ptr cTm_mday
    (#poke struct tm, tm_mon)   ptr cTm_mon
    (#poke struct tm, tm_year)  ptr cTm_year
    (#poke struct tm, tm_wday)  ptr cTm_wday
    (#poke struct tm, tm_yday)  ptr cTm_yday
    (#poke struct tm, tm_isdst) ptr cTm_isdst
