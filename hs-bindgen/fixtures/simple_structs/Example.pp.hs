{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure)

{-| __C declaration:__ @S1@

    __defined at:__ @simple_structs.h:2:8@

    __exported by:__ @simple_structs.h@
-}
data S1 = S1
  { s1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:3:9@

         __exported by:__ @simple_structs.h@
    -}
  , s1_b :: FC.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:4:10@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s1_b3

{-| __C declaration:__ @S2_t@

    __defined at:__ @simple_structs.h:8:16@

    __exported by:__ @simple_structs.h@
-}
data S2_t = S2_t
  { s2_t_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:9:10@

         __exported by:__ @simple_structs.h@
    -}
  , s2_t_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:10:9@

         __exported by:__ @simple_structs.h@
    -}
  , s2_t_c :: FC.CFloat
    {- ^ __C declaration:__ @c@

         __defined at:__ @simple_structs.h:11:11@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2_t where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t s2_t_a2 s2_t_b3 s2_t_c4 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_t_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s2_t_b3
            >> F.pokeByteOff ptr0 (8 :: Int) s2_t_c4

{-| __defined at:__ @simple_structs.h:15:9@

    __exported by:__ @simple_structs.h@
-}
data S3_t = S3_t
  { s3_t_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:16:10@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S3_t where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure S3_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 -> F.pokeByteOff ptr0 (0 :: Int) s3_t_a2

{-| __C declaration:__ @S4@

    __defined at:__ @simple_structs.h:19:8@

    __exported by:__ @simple_structs.h@
-}
data S4 = S4
  { s4_b :: FC.CChar
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:20:10@

         __exported by:__ @simple_structs.h@
    -}
  , s4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:21:9@

         __exported by:__ @simple_structs.h@
    -}
  , s4_c :: Ptr.Ptr FC.CInt
    {- ^ __C declaration:__ @c@

         __defined at:__ @simple_structs.h:22:10@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S4 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S4
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               F.pokeByteOff ptr0 (0 :: Int) s4_b2
            >> F.pokeByteOff ptr0 (4 :: Int) s4_a3
            >> F.pokeByteOff ptr0 (8 :: Int) s4_c4

{-| __C declaration:__ @S5@

    __defined at:__ @simple_structs.h:26:16@

    __exported by:__ @simple_structs.h@
-}
data S5 = S5
  { s5_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:27:10@

         __exported by:__ @simple_structs.h@
    -}
  , s5_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:28:9@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S5 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S5
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s5_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s5_b3

{-| __C declaration:__ @S6@

    __defined at:__ @simple_structs.h:31:8@

    __exported by:__ @simple_structs.h@
-}
data S6 = S6
  { s6_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:31:18@

         __exported by:__ @simple_structs.h@
    -}
  , s6_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:31:25@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S6 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S6
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s6_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s6_b3

{-| __defined at:__ @simple_structs.h:34:9@

    __exported by:__ @simple_structs.h@
-}
data S7a_Deref = S7a_Deref
  { s7a_Deref_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:34:23@

         __exported by:__ @simple_structs.h@
    -}
  , s7a_Deref_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:34:30@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S7a_Deref where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S7a_Deref
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Deref s7a_Deref_a2 s7a_Deref_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s7a_Deref_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s7a_Deref_b3

{-| __C declaration:__ @S7a@

    __defined at:__ @simple_structs.h:34:36@

    __exported by:__ @simple_structs.h@
-}
newtype S7a = S7a
  { un_S7a :: Ptr.Ptr S7a_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __defined at:__ @simple_structs.h:35:9@

    __exported by:__ @simple_structs.h@
-}
data S7b_Deref = S7b_Deref
  { s7b_Deref_a :: FC.CChar
    {- ^ __C declaration:__ @a@

         __defined at:__ @simple_structs.h:35:23@

         __exported by:__ @simple_structs.h@
    -}
  , s7b_Deref_b :: FC.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @simple_structs.h:35:30@

         __exported by:__ @simple_structs.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S7b_Deref where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S7b_Deref
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Deref s7b_Deref_a2 s7b_Deref_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s7b_Deref_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s7b_Deref_b3

{-| __C declaration:__ @S7b@

    __defined at:__ @simple_structs.h:35:38@

    __exported by:__ @simple_structs.h@
-}
newtype S7b = S7b
  { un_S7b :: Ptr.Ptr (Ptr.Ptr (Ptr.Ptr S7b_Deref))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
