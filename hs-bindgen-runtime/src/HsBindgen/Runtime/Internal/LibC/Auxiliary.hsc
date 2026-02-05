{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UnboxedTuples #-}

-- | C standard library types that are not in @base@
--
-- These are re-exported in the public-facing module "HsBindgen.Runtime.LibC".
module HsBindgen.Runtime.Internal.LibC.Auxiliary (
    -- * Floating Types
    CFenvT
  , CFexceptT

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
import Data.Primitive.Types (Prim)
import Data.Proxy (Proxy (..))
import Data.Word (Word16, Word32)
import Foreign.C.Types qualified as C
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Records (HasField (..))

import HsBindgen.Runtime.HasCField (HasCField (..))
import HsBindgen.Runtime.HasCField qualified as HasCField
import HsBindgen.Runtime.Internal.Bitfield (Bitfield)
import HsBindgen.Runtime.Internal.HasFFIType (HasFFIType)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
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

--------------------------------------------------------------------------------

-- | C @fenv_t@ type
--
-- @fenv_t@ represents the entire floating-point environment.  It is
-- implementation-specific, so this representation is opaque and may only be
-- used with a 'Ptr'.  It is available since C99.  It is defined in the @fenv.h@
-- header file.
data CFenvT

--------------------------------------------------------------------------------

-- | C @fexcept_t@ type
--
-- @fexcept_t@ represents the floating-point status flags collectively,
-- including any status the implementation associates with the flags.  It is
-- implementation-specific, so this representation is opaque and may only be
-- used with a 'Ptr'.  It is available since C99.  It is defined in the @fenv.h@
-- header file.
data CFexceptT

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
      Bitfield
    , Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , HasFFIType
    , Integral
    , Ix
    , Num
    , Ord
    , Prim
    , Read
    , ReadRaw
    , Real
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

--------------------------------------------------------------------------------

-- | C @mbstate_t@ type
--
-- @mbstate_t@ is a complete object type other than an array type that can hold
-- the conversion state information necessary to convert between sequences of
-- multibyte characters and wide characters.  It is implementation-specific, so
-- this representation is opaque and may only be used with a 'Ptr'.  It is
-- available since C95.  It is defined in the @wchar.h@ and @uchar.h@ header
-- files.
data CMbstateT

--------------------------------------------------------------------------------

-- | C @wctrans_t@ type
--
-- @wctrans_t@ is a scalar type that can hold values which represent
-- locale-specific character transformations.  It is available since C95.  It is
-- defined in the @wctype.h@ header file.
newtype CWctransT = CWctransT (Ptr C.CInt)
  deriving newtype (
      Eq
    , HasFFIType
    , Prim
    , ReadRaw
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

--------------------------------------------------------------------------------

-- | C @wctype_t@ type
--
-- @wctype_t@ is a scalar type that can hold values which represent
-- locale-specific character classification categories.  It is available since
-- C95.  It is defined in the @wctype.h@ and @wchar.h@ header files.
newtype CWctypeT = CWctypeT C.CULong
  deriving newtype (
      Eq
    , HasFFIType
    , Prim
    , ReadRaw
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

--------------------------------------------------------------------------------

-- | C @char16_t@ type
--
-- @char16_t@ represents a 16-bit Unicode character.  It is available since C11.
-- It is defined in the @uchar.h@ header file.
newtype CChar16T = CChar16T Word16
  deriving newtype (
      Bitfield
    , Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , HasFFIType
    , Integral
    , Ix
    , Num
    , Ord
    , Prim
    , Read
    , ReadRaw
    , Real
    , Show
    , StaticSize
    , Storable
    , WriteRaw
    )

--------------------------------------------------------------------------------

-- | C @char32_t@ type
--
-- @char32_t@ represents a 32-bit Unicode character.  It is available since C11.
-- It is defined in the @uchar.h@ header file.
newtype CChar32T = CChar32T Word32
  deriving newtype (
      Bitfield
    , Bits
    , Bounded
    , Enum
    , Eq
    , FiniteBits
    , HasFFIType
    , Integral
    , Ix
    , Num
    , Ord
    , Prim
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
      tm_sec   :: C.CInt -- ^ Seconds after the minute (@[0, 61]@)
    , tm_min   :: C.CInt -- ^ Minutes after the hour (@[0, 59]@)
    , tm_hour  :: C.CInt -- ^ Hours since midnight (@[0, 23]@)
    , tm_mday  :: C.CInt -- ^ Day of the month (@[1, 31]@)
    , tm_mon   :: C.CInt -- ^ Months since January (@[0, 11]@)
    , tm_year  :: C.CInt -- ^ Years since 1900
    , tm_wday  :: C.CInt -- ^ Days since Sunday (@[0, 6]@)
    , tm_yday  :: C.CInt -- ^ Days since January 1 (@[0, 365]@)
    , tm_isdst :: C.CInt -- ^ Daylight Saving Time flag
    }
  deriving stock (Eq, Show)

instance HasCField CTm "tm_sec" where
  type CFieldType CTm "tm_sec" = C.CInt
  offset## _ _ = #offset struct tm, tm_sec

instance HasCField CTm "tm_min" where
  type CFieldType CTm "tm_min" = C.CInt
  offset## _ _ = #offset struct tm, tm_min

instance HasCField CTm "tm_hour" where
  type CFieldType CTm "tm_hour" = C.CInt
  offset## _ _ = #offset struct tm, tm_hour

instance HasCField CTm "tm_mday" where
  type CFieldType CTm "tm_mday" = C.CInt
  offset## _ _ = #offset struct tm, tm_mday

instance HasCField CTm "tm_mon" where
  type CFieldType CTm "tm_mon" = C.CInt
  offset## _ _ = #offset struct tm, tm_mon

instance HasCField CTm "tm_year" where
  type CFieldType CTm "tm_year" = C.CInt
  offset## _ _ = #offset struct tm, tm_year

instance HasCField CTm "tm_wday" where
  type CFieldType CTm "tm_wday" = C.CInt
  offset## _ _ = #offset struct tm, tm_wday

instance HasCField CTm "tm_yday" where
  type CFieldType CTm "tm_yday" = C.CInt
  offset## _ _ = #offset struct tm, tm_yday

instance HasCField CTm "tm_isdst" where
  type CFieldType CTm "tm_isdst" = C.CInt
  offset## _ _ = #offset struct tm, tm_isdst

instance ( TyEq ty (CFieldType CTm "tm_sec")
         ) => HasField "tm_sec" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_sec")

instance ( TyEq ty (CFieldType CTm "tm_min")
         ) => HasField "tm_min" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_min")

instance ( TyEq ty (CFieldType CTm "tm_hour")
         ) => HasField "tm_hour" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_hour")

instance ( TyEq ty (CFieldType CTm "tm_mday")
         ) => HasField "tm_mday" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_mday")

instance ( TyEq ty (CFieldType CTm "tm_mon")
         ) => HasField "tm_mon" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_mon")

instance ( TyEq ty (CFieldType CTm "tm_year")
         ) => HasField "tm_year" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_year")

instance ( TyEq ty (CFieldType CTm "tm_wday")
         ) => HasField "tm_wday" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_wday")

instance ( TyEq ty (CFieldType CTm "tm_yday")
         ) => HasField "tm_yday" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_yday")

instance ( TyEq ty (CFieldType CTm "tm_isdst")
         ) => HasField "tm_isdst" (Ptr CTm) (Ptr ty) where
  getField = HasCField.fromPtr (Proxy @"tm_isdst")

instance ReadRaw CTm where
  readRaw ptr = do
    tm_sec   <- (#peek struct tm, tm_sec)   ptr
    tm_min   <- (#peek struct tm, tm_min)   ptr
    tm_hour  <- (#peek struct tm, tm_hour)  ptr
    tm_mday  <- (#peek struct tm, tm_mday)  ptr
    tm_mon   <- (#peek struct tm, tm_mon)   ptr
    tm_year  <- (#peek struct tm, tm_year)  ptr
    tm_wday  <- (#peek struct tm, tm_wday)  ptr
    tm_yday  <- (#peek struct tm, tm_yday)  ptr
    tm_isdst <- (#peek struct tm, tm_isdst) ptr
    return CTm{..}

instance StaticSize CTm where
  staticSizeOf    _ = #size      struct tm
  staticAlignment _ = #alignment struct tm
