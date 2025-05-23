{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Vector where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

-- #include "vector.h"
-- struct <anon> *Vector_new_vector (double arg1, double arg2);

data Vector = Vector
  { vector_x :: FC.CDouble
  , vector_y :: FC.CDouble
  }

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

deriving stock instance Show Vector

deriving stock instance Eq Vector

foreign import capi safe "vector.h new_vector" new_vector :: FC.CDouble -> FC.CDouble -> IO (F.Ptr Vector)
