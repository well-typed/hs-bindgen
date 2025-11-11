{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct2@

    __defined at:__ @skip_over_long_double.h:13:8@

    __exported by:__ @skip_over_long_double.h@
-}
data Struct2 = Struct2
  { struct2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @skip_over_long_double.h:14:7@

         __exported by:__ @skip_over_long_double.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_x2
