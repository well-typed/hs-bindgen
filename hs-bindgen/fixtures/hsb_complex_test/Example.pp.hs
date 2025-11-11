{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __defined at:__ @hsb_complex_test.h:24:9@

    __exported by:__ @hsb_complex_test.h@
-}
data Complex_object_t = Complex_object_t
  { complex_object_t_velocity :: Data.Complex.Complex FC.CFloat
    {- ^ __C declaration:__ @velocity@

         __defined at:__ @hsb_complex_test.h:25:20@

         __exported by:__ @hsb_complex_test.h@
    -}
  , complex_object_t_position :: Data.Complex.Complex FC.CDouble
    {- ^ __C declaration:__ @position@

         __defined at:__ @hsb_complex_test.h:26:20@

         __exported by:__ @hsb_complex_test.h@
    -}
  , complex_object_t_id :: FC.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @hsb_complex_test.h:27:9@

         __exported by:__ @hsb_complex_test.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Complex_object_t where

  sizeOf = \_ -> (32 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Complex_object_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Complex_object_t
            complex_object_t_velocity2
            complex_object_t_position3
            complex_object_t_id4 ->
                 F.pokeByteOff ptr0 (0 :: Int) complex_object_t_velocity2
              >> F.pokeByteOff ptr0 (8 :: Int) complex_object_t_position3
              >> F.pokeByteOff ptr0 (24 :: Int) complex_object_t_id4
