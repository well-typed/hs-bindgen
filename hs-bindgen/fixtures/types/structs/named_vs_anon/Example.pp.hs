{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import Prelude (Eq, Int, Show, pure, return)

{-| __C declaration:__ @a@

    __defined at:__ @types\/structs\/named_vs_anon.h:15:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data A = A
  {}
  deriving stock (Eq, Show)

instance F.Storable A where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure A

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A -> return ()

{-| __C declaration:__ @struct1@

    __defined at:__ @types\/structs\/named_vs_anon.h:16:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct1 = Struct1
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 -> return ()

{-| __C declaration:__ @b_s@

    __defined at:__ @types\/structs\/named_vs_anon.h:19:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data B_s = B_s
  {}
  deriving stock (Eq, Show)

instance F.Storable B_s where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure B_s

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          B_s -> return ()

{-| __C declaration:__ @struct2_s@

    __defined at:__ @types\/structs\/named_vs_anon.h:20:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct2_s = Struct2_s
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct2_s where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct2_s

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_s -> return ()

{-| __C declaration:__ @c@

    __defined at:__ @types\/structs\/named_vs_anon.h:23:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data C = C
  {}
  deriving stock (Eq, Show)

instance F.Storable C where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure C

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          C -> return ()

{-| __C declaration:__ @struct3@

    __defined at:__ @types\/structs\/named_vs_anon.h:24:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct3 = Struct3
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct3

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 -> return ()

{-| __C declaration:__ @d@

    __defined at:__ @types\/structs\/named_vs_anon.h:27:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data D = D
  {}
  deriving stock (Eq, Show)

instance F.Storable D where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure D

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          D -> return ()

{-| __C declaration:__ @struct4@

    __defined at:__ @types\/structs\/named_vs_anon.h:28:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct4 = Struct4
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 -> return ()

{-| __C declaration:__ @e_s@

    __defined at:__ @types\/structs\/named_vs_anon.h:31:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data E_s = E_s
  {}
  deriving stock (Eq, Show)

instance F.Storable E_s where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure E_s

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          E_s -> return ()

{-| __C declaration:__ @struct5_s@

    __defined at:__ @types\/structs\/named_vs_anon.h:32:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct5_s = Struct5_s
  {}
  deriving stock (Eq, Show)

instance F.Storable Struct5_s where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Struct5_s

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct5_s -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:35:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data F = F
  {}
  deriving stock (Eq, Show)

instance F.Storable F where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure F

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          F -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:36:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef1 = Typedef1
  {}
  deriving stock (Eq, Show)

instance F.Storable Typedef1 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Typedef1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef1 -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:39:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data G = G
  {}
  deriving stock (Eq, Show)

instance F.Storable G where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure G

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          G -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:40:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef2 = Typedef2
  {}
  deriving stock (Eq, Show)

instance F.Storable Typedef2 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Typedef2

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef2 -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:43:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data H = H
  {}
  deriving stock (Eq, Show)

instance F.Storable H where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure H

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          H -> return ()

{-| __defined at:__ @types\/structs\/named_vs_anon.h:44:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef3 = Typedef3
  {}
  deriving stock (Eq, Show)

instance F.Storable Typedef3 where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Typedef3

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef3 -> return ()
