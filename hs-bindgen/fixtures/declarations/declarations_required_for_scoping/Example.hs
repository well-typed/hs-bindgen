{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __C declaration:__ @A@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 5:9@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
newtype A = A
  { un_A :: HsBindgen.Runtime.LibC.CSize
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasFFIType.HasFFIType, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
