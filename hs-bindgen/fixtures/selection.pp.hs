{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @Baz@

    __defined at:__ @selection.h:1:8@

    __exported by:__ @selection.h@
-}
data Baz = Baz
  { baz_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @selection.h:2:7@

         __exported by:__ @selection.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Baz
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_x2 -> F.pokeByteOff ptr0 (0 :: Int) baz_x2
