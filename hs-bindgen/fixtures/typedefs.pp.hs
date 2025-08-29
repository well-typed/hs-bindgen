{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

{-| __/Automatically generated from C/__

    __C declaration:__ @myint@

    __defined at:__ @typedefs.h:1:13@

    __exported by:__ @typedefs.h@
-}
newtype Myint = Myint
  { un_Myint :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __/Automatically generated from C/__

    __C declaration:__ @intptr@

    __defined at:__ @typedefs.h:2:15@

    __exported by:__ @typedefs.h@
-}
newtype Intptr = Intptr
  { un_Intptr :: F.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
