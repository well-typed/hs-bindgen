{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @b@

    __defined at:__ @circular_dependency_struct.h:3:8@

    __exported by:__ @circular_dependency_struct.h@
-}
data B = B
  { b_toA :: Ptr.Ptr A
    {- ^ __C declaration:__ @toA@

         __defined at:__ @circular_dependency_struct.h:4:13@

         __exported by:__ @circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable B where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure B
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_toA2 -> F.pokeByteOff ptr0 (0 :: Int) b_toA2

{-| __C declaration:__ @a@

    __defined at:__ @circular_dependency_struct.h:7:8@

    __exported by:__ @circular_dependency_struct.h@
-}
data A = A
  { a_toB :: B
    {- ^ __C declaration:__ @toB@

         __defined at:__ @circular_dependency_struct.h:8:12@

         __exported by:__ @circular_dependency_struct.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable A where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure A
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A a_toB2 -> F.pokeByteOff ptr0 (0 :: Int) a_toB2
