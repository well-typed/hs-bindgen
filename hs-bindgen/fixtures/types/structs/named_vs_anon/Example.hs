{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct a@

    __defined at:__ @types\/structs\/named_vs_anon.h 15:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data A = A
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize A where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw A where

  readRaw = \ptr0 -> pure A

instance Marshal.WriteRaw A where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          A -> return ()

deriving via Marshal.EquivStorable A instance RIP.Storable A

{-| __C declaration:__ @struct struct1@

    __defined at:__ @types\/structs\/named_vs_anon.h 16:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct1 = Struct1
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct1 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct1 where

  readRaw = \ptr0 -> pure Struct1

instance Marshal.WriteRaw Struct1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 -> return ()

deriving via Marshal.EquivStorable Struct1 instance RIP.Storable Struct1

{-| __C declaration:__ @struct b_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 19:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data B_s = B_s
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize B_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw B_s where

  readRaw = \ptr0 -> pure B_s

instance Marshal.WriteRaw B_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B_s -> return ()

deriving via Marshal.EquivStorable B_s instance RIP.Storable B_s

{-| __C declaration:__ @struct struct2_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 20:8@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct2_s = Struct2_s
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct2_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct2_s where

  readRaw = \ptr0 -> pure Struct2_s

instance Marshal.WriteRaw Struct2_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_s -> return ()

deriving via Marshal.EquivStorable Struct2_s instance RIP.Storable Struct2_s

{-| __C declaration:__ @struct c@

    __defined at:__ @types\/structs\/named_vs_anon.h 23:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data C = C
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize C where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw C where

  readRaw = \ptr0 -> pure C

instance Marshal.WriteRaw C where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          C -> return ()

deriving via Marshal.EquivStorable C instance RIP.Storable C

{-| __C declaration:__ @struct struct3@

    __defined at:__ @types\/structs\/named_vs_anon.h 24:36@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct3 = Struct3
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct3 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct3 where

  readRaw = \ptr0 -> pure Struct3

instance Marshal.WriteRaw Struct3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 -> return ()

deriving via Marshal.EquivStorable Struct3 instance RIP.Storable Struct3

{-| __C declaration:__ @struct d@

    __defined at:__ @types\/structs\/named_vs_anon.h 27:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data D = D
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize D where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw D where

  readRaw = \ptr0 -> pure D

instance Marshal.WriteRaw D where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          D -> return ()

deriving via Marshal.EquivStorable D instance RIP.Storable D

{-| __C declaration:__ @struct struct4@

    __defined at:__ @types\/structs\/named_vs_anon.h 28:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct4 = Struct4
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct4 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct4 where

  readRaw = \ptr0 -> pure Struct4

instance Marshal.WriteRaw Struct4 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 -> return ()

deriving via Marshal.EquivStorable Struct4 instance RIP.Storable Struct4

{-| __C declaration:__ @struct e_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 31:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data E_s = E_s
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize E_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw E_s where

  readRaw = \ptr0 -> pure E_s

instance Marshal.WriteRaw E_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E_s -> return ()

deriving via Marshal.EquivStorable E_s instance RIP.Storable E_s

{-| __C declaration:__ @struct struct5_s@

    __defined at:__ @types\/structs\/named_vs_anon.h 32:15@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Struct5_s = Struct5_s
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Struct5_s where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Struct5_s where

  readRaw = \ptr0 -> pure Struct5_s

instance Marshal.WriteRaw Struct5_s where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct5_s -> return ()

deriving via Marshal.EquivStorable Struct5_s instance RIP.Storable Struct5_s

{-| __C declaration:__ @struct f@

    __defined at:__ @types\/structs\/named_vs_anon.h 35:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data F = F
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize F where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw F where

  readRaw = \ptr0 -> pure F

instance Marshal.WriteRaw F where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          F -> return ()

deriving via Marshal.EquivStorable F instance RIP.Storable F

{-| __C declaration:__ @struct typedef1@

    __defined at:__ @types\/structs\/named_vs_anon.h 36:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef1 = Typedef1
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Typedef1 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Typedef1 where

  readRaw = \ptr0 -> pure Typedef1

instance Marshal.WriteRaw Typedef1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef1 -> return ()

deriving via Marshal.EquivStorable Typedef1 instance RIP.Storable Typedef1

{-| __C declaration:__ @struct g@

    __defined at:__ @types\/structs\/named_vs_anon.h 39:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data G = G
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize G where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw G where

  readRaw = \ptr0 -> pure G

instance Marshal.WriteRaw G where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          G -> return ()

deriving via Marshal.EquivStorable G instance RIP.Storable G

{-| __C declaration:__ @struct typedef2@

    __defined at:__ @types\/structs\/named_vs_anon.h 40:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef2 = Typedef2
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Typedef2 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Typedef2 where

  readRaw = \ptr0 -> pure Typedef2

instance Marshal.WriteRaw Typedef2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef2 -> return ()

deriving via Marshal.EquivStorable Typedef2 instance RIP.Storable Typedef2

{-| __C declaration:__ @struct h@

    __defined at:__ @types\/structs\/named_vs_anon.h 43:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data H = H
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize H where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw H where

  readRaw = \ptr0 -> pure H

instance Marshal.WriteRaw H where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          H -> return ()

deriving via Marshal.EquivStorable H instance RIP.Storable H

{-| __C declaration:__ @struct typedef3@

    __defined at:__ @types\/structs\/named_vs_anon.h 44:9@

    __exported by:__ @types\/structs\/named_vs_anon.h@
-}
data Typedef3 = Typedef3
  {}
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Typedef3 where

  staticSizeOf = \_ -> (0 :: Int)

  staticAlignment = \_ -> (1 :: Int)

instance Marshal.ReadRaw Typedef3 where

  readRaw = \ptr0 -> pure Typedef3

instance Marshal.WriteRaw Typedef3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Typedef3 -> return ()

deriving via Marshal.EquivStorable Typedef3 instance RIP.Storable Typedef3
