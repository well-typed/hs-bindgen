{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnboxedTuples #-}

module Example where

import qualified Data.Primitive.Types
import qualified Foreign as F
import GHC.Prim (Int#)
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

instance Data.Primitive.Types.Prim A where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> A

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, A #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              A -> s3

  indexOffAddr# = \addr0 -> \i1 -> A

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, A #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              A -> s3

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

instance Data.Primitive.Types.Prim Struct1 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Struct1

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct1 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct1

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct1 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct1 -> s3

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

instance Data.Primitive.Types.Prim B_s where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> B_s

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, B_s #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              B_s -> s3

  indexOffAddr# = \addr0 -> \i1 -> B_s

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, B_s #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              B_s -> s3

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

instance Data.Primitive.Types.Prim Struct2_s where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Struct2_s

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct2_s #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_s -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct2_s

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct2_s #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct2_s -> s3

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

instance Data.Primitive.Types.Prim C where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> C

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, C #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              C -> s3

  indexOffAddr# = \addr0 -> \i1 -> C

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, C #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              C -> s3

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

instance Data.Primitive.Types.Prim Struct3 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Struct3

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct3 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct3 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct3

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct3 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct3 -> s3

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

instance Data.Primitive.Types.Prim D where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> D

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, D #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              D -> s3

  indexOffAddr# = \addr0 -> \i1 -> D

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, D #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              D -> s3

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

instance Data.Primitive.Types.Prim Struct4 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Struct4

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct4 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct4

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct4 -> s3

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

instance Data.Primitive.Types.Prim E_s where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> E_s

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, E_s #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              E_s -> s3

  indexOffAddr# = \addr0 -> \i1 -> E_s

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, E_s #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              E_s -> s3

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

instance Data.Primitive.Types.Prim Struct5_s where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Struct5_s

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Struct5_s #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct5_s -> s3

  indexOffAddr# = \addr0 -> \i1 -> Struct5_s

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Struct5_s #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Struct5_s -> s3

{-| __C declaration:__ @f@

    __defined at:__ @types\/structs\/named_vs_anon.h:35:9@

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

instance Data.Primitive.Types.Prim F where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> F

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, F #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              F -> s3

  indexOffAddr# = \addr0 -> \i1 -> F

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, F #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              F -> s3

{-| __C declaration:__ @typedef1@

    __defined at:__ @types\/structs\/named_vs_anon.h:36:9@

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

instance Data.Primitive.Types.Prim Typedef1 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Typedef1

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Typedef1 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef1 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Typedef1

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Typedef1 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef1 -> s3

{-| __C declaration:__ @g@

    __defined at:__ @types\/structs\/named_vs_anon.h:39:9@

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

instance Data.Primitive.Types.Prim G where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> G

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, G #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              G -> s3

  indexOffAddr# = \addr0 -> \i1 -> G

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, G #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              G -> s3

{-| __C declaration:__ @typedef2@

    __defined at:__ @types\/structs\/named_vs_anon.h:40:9@

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

instance Data.Primitive.Types.Prim Typedef2 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Typedef2

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Typedef2 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef2 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Typedef2

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Typedef2 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef2 -> s3

{-| __C declaration:__ @h@

    __defined at:__ @types\/structs\/named_vs_anon.h:43:9@

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

instance Data.Primitive.Types.Prim H where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> H

  readByteArray# = \arr0 -> \i1 -> \s2 -> (# s2, H #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              H -> s3

  indexOffAddr# = \addr0 -> \i1 -> H

  readOffAddr# = \addr0 -> \i1 -> \s2 -> (# s2, H #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              H -> s3

{-| __C declaration:__ @typedef3@

    __defined at:__ @types\/structs\/named_vs_anon.h:44:9@

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

instance Data.Primitive.Types.Prim Typedef3 where

  sizeOf# = \_ -> (0# :: Int#)

  alignment# = \_ -> (1# :: Int#)

  indexByteArray# = \arr0 -> \i1 -> Typedef3

  readByteArray# =
    \arr0 -> \i1 -> \s2 -> (# s2, Typedef3 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef3 -> s3

  indexOffAddr# = \addr0 -> \i1 -> Typedef3

  readOffAddr# =
    \addr0 -> \i1 -> \s2 -> (# s2, Typedef3 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Typedef3 -> s3
