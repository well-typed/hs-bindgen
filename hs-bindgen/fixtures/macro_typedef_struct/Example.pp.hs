{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @MY_TYPE@

    __defined at:__ @macro_typedef_struct.h:1:9@

    __exported by:__ @macro_typedef_struct.h@
-}
newtype MY_TYPE = MY_TYPE
  { un_MY_TYPE :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __defined at:__ @macro_typedef_struct.h:3:9@

    __exported by:__ @macro_typedef_struct.h@
-}
data Bar = Bar
  { bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @macro_typedef_struct.h:4:7@

         __exported by:__ @macro_typedef_struct.h@
    -}
  , bar_y :: MY_TYPE
    {- ^ __C declaration:__ @y@

         __defined at:__ @macro_typedef_struct.h:5:11@

         __exported by:__ @macro_typedef_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 bar_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_x2
            >> F.pokeByteOff ptr0 (4 :: Int) bar_y3
