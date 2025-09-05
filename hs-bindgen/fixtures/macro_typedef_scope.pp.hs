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

{-| __C declaration:__ @T1@

    __defined at:__ @macro_typedef_scope.h:4:9@

    __exported by:__ @macro_typedef_scope.h@
-}
newtype T1 = T1
  { un_T1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T2@

    __defined at:__ @macro_typedef_scope.h:5:12@

    __exported by:__ @macro_typedef_scope.h@
-}
newtype T2 = T2
  { un_T2 :: T1
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T3@

    __defined at:__ @macro_typedef_scope.h:6:9@

    __exported by:__ @macro_typedef_scope.h@
-}
newtype T3 = T3
  { un_T3 :: T2
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @T4@

    __defined at:__ @macro_typedef_scope.h:7:12@

    __exported by:__ @macro_typedef_scope.h@
-}
newtype T4 = T4
  { un_T4 :: T3
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
