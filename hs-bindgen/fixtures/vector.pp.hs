{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <vector.h>\nvector *hs_bindgen_test_vector_72a6c90b1b14a9b0 (double arg1, double arg2) { return new_vector(arg1, arg2); }\n")

data Vector = Vector
  { vector_x :: FC.CDouble
  , vector_y :: FC.CDouble
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

foreign import ccall safe "hs_bindgen_test_vector_72a6c90b1b14a9b0" new_vector
  :: FC.CDouble
     {- ^ __from C:__ @x@ -}
  -> FC.CDouble
     {- ^ __from C:__ @y@ -}
  -> IO (F.Ptr Vector)
