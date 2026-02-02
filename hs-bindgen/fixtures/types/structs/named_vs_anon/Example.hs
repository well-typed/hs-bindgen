{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnboxedTuples #-}

module Example where

import qualified Data.Primitive.Types
import qualified Foreign as F
import qualified HsBindgen.Runtime.Marshal
import Prelude (Eq, Int, Show, pure, return)

{-| __C declaration:__ @struct a@

    __defined at:__ @types\/structs\/named_vs_anon.h 15:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data A = A
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize A where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw A where

  readRaw = \ptr0 -> pure A

instance HsBindgen.Runtime.Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable A instance F.Storable A

instance Data.Primitive.Types.Prim A where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct struct1@

    __defined at:__ @types\/structs\/named_vs_anon.h 16:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct1 = Struct1
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct1 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct1 where

  readRaw = \ptr0 -> pure Struct1

instance HsBindgen.Runtime.Marshal.WriteRaw Struct1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct1 instance F.Storable Struct1

instance Data.Primitive.Types.Prim Struct1 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct b_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 19:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data B_s = B_s
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize B_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw B_s where

  readRaw = \ptr0 -> pure B_s

instance HsBindgen.Runtime.Marshal.WriteRaw B_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B_s -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable B_s instance F.Storable B_s

instance Data.Primitive.Types.Prim B_s where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct struct2_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 20:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct2_s = Struct2_s
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct2_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct2_s where

  readRaw = \ptr0 -> pure Struct2_s

instance HsBindgen.Runtime.Marshal.WriteRaw Struct2_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_s -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct2_s instance F.Storable Struct2_s

instance Data.Primitive.Types.Prim Struct2_s where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct c@

    __defined at:__ @types\/structs\/named_vs_anon.h 23:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data C = C
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize C where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw C where

  readRaw = \ptr0 -> pure C

instance HsBindgen.Runtime.Marshal.WriteRaw C where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          C -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable C instance F.Storable C

instance Data.Primitive.Types.Prim C where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct struct3@

    __defined at:__ @types\/structs\/named_vs_anon.h 24:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct3 = Struct3
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct3 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct3 where

  readRaw = \ptr0 -> pure Struct3

instance HsBindgen.Runtime.Marshal.WriteRaw Struct3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct3 instance F.Storable Struct3

instance Data.Primitive.Types.Prim Struct3 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct d@

    __defined at:__ @types\/structs\/named_vs_anon.h 27:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data D = D
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize D where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw D where

  readRaw = \ptr0 -> pure D

instance HsBindgen.Runtime.Marshal.WriteRaw D where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          D -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable D instance F.Storable D

instance Data.Primitive.Types.Prim D where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct struct4@

    __defined at:__ @types\/structs\/named_vs_anon.h 28:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct4 = Struct4
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct4 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct4 where

  readRaw = \ptr0 -> pure Struct4

instance HsBindgen.Runtime.Marshal.WriteRaw Struct4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct4 instance F.Storable Struct4

instance Data.Primitive.Types.Prim Struct4 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct e_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 31:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data E_s = E_s
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize E_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw E_s where

  readRaw = \ptr0 -> pure E_s

instance HsBindgen.Runtime.Marshal.WriteRaw E_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E_s -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable E_s instance F.Storable E_s

instance Data.Primitive.Types.Prim E_s where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct struct5_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 32:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct5_s = Struct5_s
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Struct5_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Struct5_s where

  readRaw = \ptr0 -> pure Struct5_s

instance HsBindgen.Runtime.Marshal.WriteRaw Struct5_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct5_s -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Struct5_s instance F.Storable Struct5_s

instance Data.Primitive.Types.Prim Struct5_s where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct f@

    __defined at:__ @types\/structs\/named_vs_anon.h 35:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data F = F
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize F where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw F where

  readRaw = \ptr0 -> pure F

instance HsBindgen.Runtime.Marshal.WriteRaw F where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          F -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable F instance F.Storable F

instance Data.Primitive.Types.Prim F where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct typedef1@

    __defined at:__ @types\/structs\/named_vs_anon.h 36:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef1 = Typedef1
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Typedef1 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Typedef1 where

  readRaw = \ptr0 -> pure Typedef1

instance HsBindgen.Runtime.Marshal.WriteRaw Typedef1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef1 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Typedef1 instance F.Storable Typedef1

instance Data.Primitive.Types.Prim Typedef1 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct g@

    __defined at:__ @types\/structs\/named_vs_anon.h 39:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data G = G
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize G where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw G where

  readRaw = \ptr0 -> pure G

instance HsBindgen.Runtime.Marshal.WriteRaw G where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          G -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable G instance F.Storable G

instance Data.Primitive.Types.Prim G where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct typedef2@

    __defined at:__ @types\/structs\/named_vs_anon.h 40:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef2 = Typedef2
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Typedef2 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Typedef2 where

  readRaw = \ptr0 -> pure Typedef2

instance HsBindgen.Runtime.Marshal.WriteRaw Typedef2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef2 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Typedef2 instance F.Storable Typedef2

instance Data.Primitive.Types.Prim Typedef2 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct h@

    __defined at:__ @types\/structs\/named_vs_anon.h 43:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data H = H
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize H where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw H where

  readRaw = \ptr0 -> pure H

instance HsBindgen.Runtime.Marshal.WriteRaw H where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          H -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable H instance F.Storable H

instance Data.Primitive.Types.Prim H where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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

{-| __C declaration:__ @struct typedef3@

    __defined at:__ @types\/structs\/named_vs_anon.h 44:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef3 = Typedef3
  {}
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Typedef3 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Typedef3 where

  readRaw = \ptr0 -> pure Typedef3

instance HsBindgen.Runtime.Marshal.WriteRaw Typedef3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef3 -> return ()

deriving via HsBindgen.Runtime.Marshal.EquivStorable Typedef3 instance F.Storable Typedef3

instance Data.Primitive.Types.Prim Typedef3 where

  sizeOf# = \_ -> (0#)

  alignment# = \_ -> (1#)

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
