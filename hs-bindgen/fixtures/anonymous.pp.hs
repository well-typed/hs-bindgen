{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __defined at:__ @anonymous.h:3:3@

    __exported by:__ @anonymous.h@
-}
data S1_c = S1_c
  { s1_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @anonymous.h:4:9@

         __exported by:__ @anonymous.h@
    -}
  , s1_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @anonymous.h:5:9@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1_c
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_c s1_c_a2 s1_c_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_c_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s1_c_b3

{-| __C declaration:__ @S1@

    __defined at:__ @anonymous.h:2:8@

    __exported by:__ @anonymous.h@
-}
data S1 = S1
  { s1_c :: S1_c
    {- ^ __C declaration:__ @c@

         __defined at:__ @anonymous.h:6:5@

         __exported by:__ @anonymous.h@
    -}
  , s1_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @anonymous.h:8:7@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_c2 s1_d3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_c2
            >> F.pokeByteOff ptr0 (8 :: Int) s1_d3

{-| __defined at:__ @anonymous.h:15:5@

    __exported by:__ @anonymous.h@
-}
data S2_inner_deep = S2_inner_deep
  { s2_inner_deep_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @anonymous.h:16:11@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner_deep where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner_deep
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner_deep s2_inner_deep_b2 ->
            F.pokeByteOff ptr0 (0 :: Int) s2_inner_deep_b2

{-| __defined at:__ @anonymous.h:13:3@

    __exported by:__ @anonymous.h@
-}
data S2_inner = S2_inner
  { s2_inner_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @anonymous.h:14:9@

         __exported by:__ @anonymous.h@
    -}
  , s2_inner_deep :: S2_inner_deep
    {- ^ __C declaration:__ @deep@

         __defined at:__ @anonymous.h:17:7@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_inner where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_inner
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_inner s2_inner_a2 s2_inner_deep3 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_inner_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s2_inner_deep3

{-| __C declaration:__ @S2@

    __defined at:__ @anonymous.h:12:8@

    __exported by:__ @anonymous.h@
-}
data S2 = S2
  { s2_inner :: S2_inner
    {- ^ __C declaration:__ @inner@

         __defined at:__ @anonymous.h:18:5@

         __exported by:__ @anonymous.h@
    -}
  , s2_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @anonymous.h:20:7@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_inner2 s2_d3 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_inner2
            >> F.pokeByteOff ptr0 (8 :: Int) s2_d3

{-| __C declaration:__ @S3@

    __defined at:__ @anonymous.h:24:8@

    __exported by:__ @anonymous.h@
-}
data S3 = S3
  { s3_c :: Ptr.Ptr (Ptr.Ptr S3_c)
    {- ^ __C declaration:__ @c@

         __defined at:__ @anonymous.h:28:7@

         __exported by:__ @anonymous.h@
    -}
  , s3_d :: FC.CInt
    {- ^ __C declaration:__ @d@

         __defined at:__ @anonymous.h:30:7@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S3
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3 s3_c2 s3_d3 ->
               F.pokeByteOff ptr0 (0 :: Int) s3_c2
            >> F.pokeByteOff ptr0 (8 :: Int) s3_d3

{-| __defined at:__ @anonymous.h:25:3@

    __exported by:__ @anonymous.h@
-}
data S3_c = S3_c
  { s3_c_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @anonymous.h:26:9@

         __exported by:__ @anonymous.h@
    -}
  , s3_c_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @anonymous.h:27:9@

         __exported by:__ @anonymous.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3_c where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S3_c
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_c s3_c_a2 s3_c_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s3_c_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s3_c_b3
