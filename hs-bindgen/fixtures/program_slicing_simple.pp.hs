{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Example
import qualified Foreign as F
import qualified Foreign.C as FC
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @uint32_t@

    __defined at:__ @bits\/alltypes.h:131:25@

    __exported by:__ @program_slicing_simple.h@
-}
newtype Uint32_t = Uint32_t
  { un_Uint32_t :: FC.CUInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @foo@

    __defined at:__ @program_slicing_simple.h:3:8@

    __exported by:__ @program_slicing_simple.h@
-}
data Foo = Foo
  { foo_sixty_four :: Example.Uint64_t
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @program_slicing_simple.h:4:12@

         __exported by:__ @program_slicing_simple.h@
    -}
  , foo_thirty_two :: Uint32_t
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @program_slicing_simple.h:5:12@

         __exported by:__ @program_slicing_simple.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_sixty_four2
            >> F.pokeByteOff ptr0 (8 :: Int) foo_thirty_two3
