{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @bools1@

    __defined at:__ @bool.h:1:8@

    __exported by:__ @bool.h@
-}
data Bools1 = Bools1
  { bools1_x :: FC.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @bool.h:2:11@

         __exported by:__ @bool.h@
    -}
  , bools1_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @bool.h:3:11@

         __exported by:__ @bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools1 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools1_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools1_y3

{-| __C declaration:__ @bools2@

    __defined at:__ @bool.h:8:8@

    __exported by:__ @bool.h@
-}
data Bools2 = Bools2
  { bools2_x :: FC.CBool
    {- ^ __C declaration:__ @x@

         __defined at:__ @bool.h:9:10@

         __exported by:__ @bool.h@
    -}
  , bools2_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @bool.h:10:10@

         __exported by:__ @bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools2 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools2_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools2_y3

{-| __C declaration:__ @BOOL@

    __defined at:__ @bool.h:13:9@

    __exported by:__ @bool.h@
-}
newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @bools3@

    __defined at:__ @bool.h:15:8@

    __exported by:__ @bool.h@
-}
data Bools3 = Bools3
  { bools3_x :: BOOL
    {- ^ __C declaration:__ @x@

         __defined at:__ @bool.h:16:10@

         __exported by:__ @bool.h@
    -}
  , bools3_y :: BOOL
    {- ^ __C declaration:__ @y@

         __defined at:__ @bool.h:17:10@

         __exported by:__ @bool.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bools3 where

  sizeOf = \_ -> (2 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bools3
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) bools3_x2
            >> F.pokeByteOff ptr0 (1 :: Int) bools3_y3
