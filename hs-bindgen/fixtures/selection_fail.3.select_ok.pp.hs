{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @OkBefore@

    __defined at:__ @selection_fail.h:1:8@

    __exported by:__ @selection_fail.h@
-}
data OkBefore = OkBefore
  { okBefore_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @selection_fail.h:2:7@

         __exported by:__ @selection_fail.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable OkBefore where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure OkBefore
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          OkBefore okBefore_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) okBefore_x2
