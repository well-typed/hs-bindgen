{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @thing@

    __defined at:__ @struct_arg.h:2:8@

    __exported by:__ @struct_arg.h@
-}
data Thing = Thing
  { thing_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @struct_arg.h:3:9@

         __exported by:__ @struct_arg.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Thing where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Thing
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) thing_x2
