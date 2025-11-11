{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __defined at:__ @vector_test.h:1:9@

    __exported by:__ @vector_test.h@
-}
data Vector = Vector
  { vector_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @vector_test.h:2:12@

         __exported by:__ @vector_test.h@
    -}
  , vector_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @vector_test.h:3:12@

         __exported by:__ @vector_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Vector where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Vector
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_x2 vector_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) vector_x2
            >> F.pokeByteOff ptr0 (8 :: Int) vector_y3
