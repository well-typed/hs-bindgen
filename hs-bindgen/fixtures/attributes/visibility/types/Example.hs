{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.S0
    , Example.S1
    , Example.S2
    , Example.S3
    , Example.S4
    , Example.S5(..)
    , Example.S6(..)
    , Example.S7(..)
    , Example.S8(..)
    , Example.S9(..)
    , Example.S10(..)
    , Example.S11(..)
    , Example.S12(..)
    , Example.S13(..)
    , Example.S14(..)
    , Example.S15(..)
    , Example.S16(..)
    , Example.S17(..)
    , Example.S18(..)
    , Example.S19(..)
    , Example.U0
    , Example.U1
    , Example.U2
    , Example.U3
    , Example.U4
    , Example.U5(..)
    , Example.get_u5_x
    , Example.set_u5_x
    , Example.U6(..)
    , Example.get_u6_x
    , Example.set_u6_x
    , Example.U7(..)
    , Example.get_u7_x
    , Example.set_u7_x
    , Example.U8(..)
    , Example.get_u8_x
    , Example.set_u8_x
    , Example.U9(..)
    , Example.get_u9_x
    , Example.set_u9_x
    , Example.U10(..)
    , Example.get_u10_x
    , Example.set_u10_x
    , Example.U11(..)
    , Example.get_u11_x
    , Example.set_u11_x
    , Example.U12(..)
    , Example.get_u12_x
    , Example.set_u12_x
    , Example.U13(..)
    , Example.get_u13_x
    , Example.set_u13_x
    , Example.U14(..)
    , Example.get_u14_x
    , Example.set_u14_x
    , Example.U15(..)
    , Example.get_u15_x
    , Example.set_u15_x
    , Example.U16(..)
    , Example.get_u16_x
    , Example.set_u16_x
    , Example.U17(..)
    , Example.get_u17_x
    , Example.set_u17_x
    , Example.U18(..)
    , Example.get_u18_x
    , Example.set_u18_x
    , Example.U19(..)
    , Example.get_u19_x
    , Example.set_u19_x
    , Example.E0
    , Example.E1
    , Example.E2
    , Example.E3
    , Example.E4
    , Example.E5(..)
    , pattern Example.X5
    , Example.E6(..)
    , pattern Example.X6
    , Example.E7(..)
    , pattern Example.X7
    , Example.E8(..)
    , pattern Example.X8
    , Example.E9(..)
    , pattern Example.X9
    , Example.E10(..)
    , pattern Example.X10
    , Example.E11(..)
    , pattern Example.X11
    , Example.E12(..)
    , pattern Example.X12
    , Example.E13(..)
    , pattern Example.X13
    , Example.E14(..)
    , pattern Example.X14
    , Example.E15(..)
    , pattern Example.X15
    , Example.E16(..)
    , pattern Example.X16
    , Example.E17(..)
    , pattern Example.X17
    , Example.E18(..)
    , pattern Example.X18
    , Example.E19(..)
    , pattern Example.X19
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct S0@

    __defined at:__ @attributes\/visibility\/types.h 13:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S0

{-| __C declaration:__ @struct S1@

    __defined at:__ @attributes\/visibility\/types.h 14:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S1

{-| __C declaration:__ @struct S2@

    __defined at:__ @attributes\/visibility\/types.h 15:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S2

{-| __C declaration:__ @struct S3@

    __defined at:__ @attributes\/visibility\/types.h 16:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S3

{-| __C declaration:__ @struct S4@

    __defined at:__ @attributes\/visibility\/types.h 17:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S4

{-| __C declaration:__ @struct S5@

    __defined at:__ @attributes\/visibility\/types.h 20:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S5 = S5
  { s5_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 20:60@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S5 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S5 where

  readRaw =
    \ptr0 ->
          pure S5
      <*> HasCField.readRaw (RIP.Proxy @"s5_x") ptr0

instance Marshal.WriteRaw S5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s5_x") ptr0 s5_x2

deriving via Marshal.EquivStorable S5 instance RIP.Storable S5

instance HasCField.HasCField S5 "s5_x" where

  type CFieldType S5 "s5_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s5_x" (RIP.Ptr S5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s5_x")

{-| __C declaration:__ @struct S6@

    __defined at:__ @attributes\/visibility\/types.h 21:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S6 = S6
  { s6_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 21:60@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S6 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S6 where

  readRaw =
    \ptr0 ->
          pure S6
      <*> HasCField.readRaw (RIP.Proxy @"s6_x") ptr0

instance Marshal.WriteRaw S6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s6_x") ptr0 s6_x2

deriving via Marshal.EquivStorable S6 instance RIP.Storable S6

instance HasCField.HasCField S6 "s6_x" where

  type CFieldType S6 "s6_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s6_x" (RIP.Ptr S6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s6_x")

{-| __C declaration:__ @struct S7@

    __defined at:__ @attributes\/visibility\/types.h 22:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S7 = S7
  { s7_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 22:60@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S7 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S7 where

  readRaw =
    \ptr0 ->
          pure S7
      <*> HasCField.readRaw (RIP.Proxy @"s7_x") ptr0

instance Marshal.WriteRaw S7 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7 s7_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s7_x") ptr0 s7_x2

deriving via Marshal.EquivStorable S7 instance RIP.Storable S7

instance HasCField.HasCField S7 "s7_x" where

  type CFieldType S7 "s7_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s7_x" (RIP.Ptr S7) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s7_x")

{-| __C declaration:__ @struct S8@

    __defined at:__ @attributes\/visibility\/types.h 23:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S8 = S8
  { s8_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 23:60@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S8 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S8 where

  readRaw =
    \ptr0 ->
          pure S8
      <*> HasCField.readRaw (RIP.Proxy @"s8_x") ptr0

instance Marshal.WriteRaw S8 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S8 s8_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s8_x") ptr0 s8_x2

deriving via Marshal.EquivStorable S8 instance RIP.Storable S8

instance HasCField.HasCField S8 "s8_x" where

  type CFieldType S8 "s8_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s8_x" (RIP.Ptr S8) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s8_x")

{-| __C declaration:__ @struct S9@

    __defined at:__ @attributes\/visibility\/types.h 24:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S9 = S9
  { s9_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 24:60@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S9 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S9 where

  readRaw =
    \ptr0 ->
          pure S9
      <*> HasCField.readRaw (RIP.Proxy @"s9_x") ptr0

instance Marshal.WriteRaw S9 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S9 s9_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s9_x") ptr0 s9_x2

deriving via Marshal.EquivStorable S9 instance RIP.Storable S9

instance HasCField.HasCField S9 "s9_x" where

  type CFieldType S9 "s9_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "s9_x" (RIP.Ptr S9) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s9_x")

{-| __C declaration:__ @struct S10@

    __defined at:__ @attributes\/visibility\/types.h 33:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S10 = S10
  { s10_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 33:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S10 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S10 where

  readRaw =
    \ptr0 ->
          pure S10
      <*> HasCField.readRaw (RIP.Proxy @"s10_x") ptr0

instance Marshal.WriteRaw S10 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S10 s10_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s10_x") ptr0 s10_x2

deriving via Marshal.EquivStorable S10 instance RIP.Storable S10

instance HasCField.HasCField S10 "s10_x" where

  type CFieldType S10 "s10_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s10_x" (RIP.Ptr S10) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s10_x")

{-| __C declaration:__ @struct S11@

    __defined at:__ @attributes\/visibility\/types.h 34:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S11 = S11
  { s11_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 34:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S11 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S11 where

  readRaw =
    \ptr0 ->
          pure S11
      <*> HasCField.readRaw (RIP.Proxy @"s11_x") ptr0

instance Marshal.WriteRaw S11 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S11 s11_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s11_x") ptr0 s11_x2

deriving via Marshal.EquivStorable S11 instance RIP.Storable S11

instance HasCField.HasCField S11 "s11_x" where

  type CFieldType S11 "s11_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s11_x" (RIP.Ptr S11) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s11_x")

{-| __C declaration:__ @struct S12@

    __defined at:__ @attributes\/visibility\/types.h 35:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S12 = S12
  { s12_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 35:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S12 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S12 where

  readRaw =
    \ptr0 ->
          pure S12
      <*> HasCField.readRaw (RIP.Proxy @"s12_x") ptr0

instance Marshal.WriteRaw S12 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S12 s12_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s12_x") ptr0 s12_x2

deriving via Marshal.EquivStorable S12 instance RIP.Storable S12

instance HasCField.HasCField S12 "s12_x" where

  type CFieldType S12 "s12_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s12_x" (RIP.Ptr S12) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s12_x")

{-| __C declaration:__ @struct S13@

    __defined at:__ @attributes\/visibility\/types.h 36:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S13 = S13
  { s13_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 36:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S13 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S13 where

  readRaw =
    \ptr0 ->
          pure S13
      <*> HasCField.readRaw (RIP.Proxy @"s13_x") ptr0

instance Marshal.WriteRaw S13 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S13 s13_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s13_x") ptr0 s13_x2

deriving via Marshal.EquivStorable S13 instance RIP.Storable S13

instance HasCField.HasCField S13 "s13_x" where

  type CFieldType S13 "s13_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s13_x" (RIP.Ptr S13) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s13_x")

{-| __C declaration:__ @struct S14@

    __defined at:__ @attributes\/visibility\/types.h 37:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S14 = S14
  { s14_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 37:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S14 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S14 where

  readRaw =
    \ptr0 ->
          pure S14
      <*> HasCField.readRaw (RIP.Proxy @"s14_x") ptr0

instance Marshal.WriteRaw S14 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S14 s14_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s14_x") ptr0 s14_x2

deriving via Marshal.EquivStorable S14 instance RIP.Storable S14

instance HasCField.HasCField S14 "s14_x" where

  type CFieldType S14 "s14_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s14_x" (RIP.Ptr S14) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s14_x")

{-| __C declaration:__ @struct S15@

    __defined at:__ @attributes\/visibility\/types.h 46:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S15 = S15
  { s15_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 46:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S15 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S15 where

  readRaw =
    \ptr0 ->
          pure S15
      <*> HasCField.readRaw (RIP.Proxy @"s15_x") ptr0

instance Marshal.WriteRaw S15 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S15 s15_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s15_x") ptr0 s15_x2

deriving via Marshal.EquivStorable S15 instance RIP.Storable S15

instance HasCField.HasCField S15 "s15_x" where

  type CFieldType S15 "s15_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s15_x" (RIP.Ptr S15) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s15_x")

{-| __C declaration:__ @struct S16@

    __defined at:__ @attributes\/visibility\/types.h 47:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S16 = S16
  { s16_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 47:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S16 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S16 where

  readRaw =
    \ptr0 ->
          pure S16
      <*> HasCField.readRaw (RIP.Proxy @"s16_x") ptr0

instance Marshal.WriteRaw S16 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S16 s16_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s16_x") ptr0 s16_x2

deriving via Marshal.EquivStorable S16 instance RIP.Storable S16

instance HasCField.HasCField S16 "s16_x" where

  type CFieldType S16 "s16_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s16_x" (RIP.Ptr S16) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s16_x")

{-| __C declaration:__ @struct S17@

    __defined at:__ @attributes\/visibility\/types.h 48:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S17 = S17
  { s17_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 48:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S17 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S17 where

  readRaw =
    \ptr0 ->
          pure S17
      <*> HasCField.readRaw (RIP.Proxy @"s17_x") ptr0

instance Marshal.WriteRaw S17 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S17 s17_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s17_x") ptr0 s17_x2

deriving via Marshal.EquivStorable S17 instance RIP.Storable S17

instance HasCField.HasCField S17 "s17_x" where

  type CFieldType S17 "s17_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s17_x" (RIP.Ptr S17) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s17_x")

{-| __C declaration:__ @struct S18@

    __defined at:__ @attributes\/visibility\/types.h 49:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S18 = S18
  { s18_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 49:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S18 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S18 where

  readRaw =
    \ptr0 ->
          pure S18
      <*> HasCField.readRaw (RIP.Proxy @"s18_x") ptr0

instance Marshal.WriteRaw S18 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S18 s18_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s18_x") ptr0 s18_x2

deriving via Marshal.EquivStorable S18 instance RIP.Storable S18

instance HasCField.HasCField S18 "s18_x" where

  type CFieldType S18 "s18_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s18_x" (RIP.Ptr S18) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s18_x")

{-| __C declaration:__ @struct S19@

    __defined at:__ @attributes\/visibility\/types.h 50:51@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data S19 = S19
  { s19_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @attributes\/visibility\/types.h 50:61@

         __exported by:__ @attributes\/visibility\/types.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize S19 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw S19 where

  readRaw =
    \ptr0 ->
          pure S19
      <*> HasCField.readRaw (RIP.Proxy @"s19_x") ptr0

instance Marshal.WriteRaw S19 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          S19 s19_x2 ->
            HasCField.writeRaw (RIP.Proxy @"s19_x") ptr0 s19_x2

deriving via Marshal.EquivStorable S19 instance RIP.Storable S19

instance HasCField.HasCField S19 "s19_x" where

  type CFieldType S19 "s19_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "s19_x" (RIP.Ptr S19) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s19_x")

{-| __C declaration:__ @union U0@

    __defined at:__ @attributes\/visibility\/types.h 53:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data U0

{-| __C declaration:__ @union U1@

    __defined at:__ @attributes\/visibility\/types.h 54:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data U1

{-| __C declaration:__ @union U2@

    __defined at:__ @attributes\/visibility\/types.h 55:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data U2

{-| __C declaration:__ @union U3@

    __defined at:__ @attributes\/visibility\/types.h 56:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data U3

{-| __C declaration:__ @union U4@

    __defined at:__ @attributes\/visibility\/types.h 57:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data U4

{-| __C declaration:__ @union U5@

    __defined at:__ @attributes\/visibility\/types.h 60:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U5 = U5
  { unwrapU5 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U5

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U5

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U5

deriving via Marshal.EquivStorable U5 instance RIP.Storable U5

{-|

    __See:__ 'set_u5_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 60:59@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u5_x ::
     U5
  -> RIP.CInt
get_u5_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u5_x'

-}
set_u5_x ::
     RIP.CInt
  -> U5
set_u5_x = RIP.setUnionPayload

instance HasCField.HasCField U5 "u5_x" where

  type CFieldType U5 "u5_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "u5_x" (RIP.Ptr U5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u5_x")

{-| __C declaration:__ @union U6@

    __defined at:__ @attributes\/visibility\/types.h 61:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U6 = U6
  { unwrapU6 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U6

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U6

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U6

deriving via Marshal.EquivStorable U6 instance RIP.Storable U6

{-|

    __See:__ 'set_u6_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 61:59@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u6_x ::
     U6
  -> RIP.CInt
get_u6_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u6_x'

-}
set_u6_x ::
     RIP.CInt
  -> U6
set_u6_x = RIP.setUnionPayload

instance HasCField.HasCField U6 "u6_x" where

  type CFieldType U6 "u6_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "u6_x" (RIP.Ptr U6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u6_x")

{-| __C declaration:__ @union U7@

    __defined at:__ @attributes\/visibility\/types.h 62:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U7 = U7
  { unwrapU7 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U7

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U7

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U7

deriving via Marshal.EquivStorable U7 instance RIP.Storable U7

{-|

    __See:__ 'set_u7_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 62:59@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u7_x ::
     U7
  -> RIP.CInt
get_u7_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u7_x'

-}
set_u7_x ::
     RIP.CInt
  -> U7
set_u7_x = RIP.setUnionPayload

instance HasCField.HasCField U7 "u7_x" where

  type CFieldType U7 "u7_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "u7_x" (RIP.Ptr U7) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u7_x")

{-| __C declaration:__ @union U8@

    __defined at:__ @attributes\/visibility\/types.h 63:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U8 = U8
  { unwrapU8 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U8

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U8

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U8

deriving via Marshal.EquivStorable U8 instance RIP.Storable U8

{-|

    __See:__ 'set_u8_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 63:59@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u8_x ::
     U8
  -> RIP.CInt
get_u8_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u8_x'

-}
set_u8_x ::
     RIP.CInt
  -> U8
set_u8_x = RIP.setUnionPayload

instance HasCField.HasCField U8 "u8_x" where

  type CFieldType U8 "u8_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "u8_x" (RIP.Ptr U8) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u8_x")

{-| __C declaration:__ @union U9@

    __defined at:__ @attributes\/visibility\/types.h 64:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U9 = U9
  { unwrapU9 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U9

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U9

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U9

deriving via Marshal.EquivStorable U9 instance RIP.Storable U9

{-|

    __See:__ 'set_u9_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 64:59@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u9_x ::
     U9
  -> RIP.CInt
get_u9_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u9_x'

-}
set_u9_x ::
     RIP.CInt
  -> U9
set_u9_x = RIP.setUnionPayload

instance HasCField.HasCField U9 "u9_x" where

  type CFieldType U9 "u9_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (ty ~ RIP.CInt) => RIP.HasField "u9_x" (RIP.Ptr U9) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u9_x")

{-| __C declaration:__ @union U10@

    __defined at:__ @attributes\/visibility\/types.h 73:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U10 = U10
  { unwrapU10 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U10

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U10

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U10

deriving via Marshal.EquivStorable U10 instance RIP.Storable U10

{-|

    __See:__ 'set_u10_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 73:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u10_x ::
     U10
  -> RIP.CInt
get_u10_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u10_x'

-}
set_u10_x ::
     RIP.CInt
  -> U10
set_u10_x = RIP.setUnionPayload

instance HasCField.HasCField U10 "u10_x" where

  type CFieldType U10 "u10_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u10_x" (RIP.Ptr U10) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u10_x")

{-| __C declaration:__ @union U11@

    __defined at:__ @attributes\/visibility\/types.h 74:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U11 = U11
  { unwrapU11 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U11

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U11

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U11

deriving via Marshal.EquivStorable U11 instance RIP.Storable U11

{-|

    __See:__ 'set_u11_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 74:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u11_x ::
     U11
  -> RIP.CInt
get_u11_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u11_x'

-}
set_u11_x ::
     RIP.CInt
  -> U11
set_u11_x = RIP.setUnionPayload

instance HasCField.HasCField U11 "u11_x" where

  type CFieldType U11 "u11_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u11_x" (RIP.Ptr U11) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u11_x")

{-| __C declaration:__ @union U12@

    __defined at:__ @attributes\/visibility\/types.h 75:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U12 = U12
  { unwrapU12 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U12

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U12

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U12

deriving via Marshal.EquivStorable U12 instance RIP.Storable U12

{-|

    __See:__ 'set_u12_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 75:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u12_x ::
     U12
  -> RIP.CInt
get_u12_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u12_x'

-}
set_u12_x ::
     RIP.CInt
  -> U12
set_u12_x = RIP.setUnionPayload

instance HasCField.HasCField U12 "u12_x" where

  type CFieldType U12 "u12_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u12_x" (RIP.Ptr U12) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u12_x")

{-| __C declaration:__ @union U13@

    __defined at:__ @attributes\/visibility\/types.h 76:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U13 = U13
  { unwrapU13 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U13

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U13

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U13

deriving via Marshal.EquivStorable U13 instance RIP.Storable U13

{-|

    __See:__ 'set_u13_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 76:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u13_x ::
     U13
  -> RIP.CInt
get_u13_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u13_x'

-}
set_u13_x ::
     RIP.CInt
  -> U13
set_u13_x = RIP.setUnionPayload

instance HasCField.HasCField U13 "u13_x" where

  type CFieldType U13 "u13_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u13_x" (RIP.Ptr U13) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u13_x")

{-| __C declaration:__ @union U14@

    __defined at:__ @attributes\/visibility\/types.h 77:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U14 = U14
  { unwrapU14 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U14

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U14

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U14

deriving via Marshal.EquivStorable U14 instance RIP.Storable U14

{-|

    __See:__ 'set_u14_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 77:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u14_x ::
     U14
  -> RIP.CInt
get_u14_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u14_x'

-}
set_u14_x ::
     RIP.CInt
  -> U14
set_u14_x = RIP.setUnionPayload

instance HasCField.HasCField U14 "u14_x" where

  type CFieldType U14 "u14_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u14_x" (RIP.Ptr U14) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u14_x")

{-| __C declaration:__ @union U15@

    __defined at:__ @attributes\/visibility\/types.h 86:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U15 = U15
  { unwrapU15 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U15

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U15

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U15

deriving via Marshal.EquivStorable U15 instance RIP.Storable U15

{-|

    __See:__ 'set_u15_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 86:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u15_x ::
     U15
  -> RIP.CInt
get_u15_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u15_x'

-}
set_u15_x ::
     RIP.CInt
  -> U15
set_u15_x = RIP.setUnionPayload

instance HasCField.HasCField U15 "u15_x" where

  type CFieldType U15 "u15_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u15_x" (RIP.Ptr U15) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u15_x")

{-| __C declaration:__ @union U16@

    __defined at:__ @attributes\/visibility\/types.h 87:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U16 = U16
  { unwrapU16 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U16

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U16

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U16

deriving via Marshal.EquivStorable U16 instance RIP.Storable U16

{-|

    __See:__ 'set_u16_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 87:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u16_x ::
     U16
  -> RIP.CInt
get_u16_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u16_x'

-}
set_u16_x ::
     RIP.CInt
  -> U16
set_u16_x = RIP.setUnionPayload

instance HasCField.HasCField U16 "u16_x" where

  type CFieldType U16 "u16_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u16_x" (RIP.Ptr U16) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u16_x")

{-| __C declaration:__ @union U17@

    __defined at:__ @attributes\/visibility\/types.h 88:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U17 = U17
  { unwrapU17 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U17

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U17

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U17

deriving via Marshal.EquivStorable U17 instance RIP.Storable U17

{-|

    __See:__ 'set_u17_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 88:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u17_x ::
     U17
  -> RIP.CInt
get_u17_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u17_x'

-}
set_u17_x ::
     RIP.CInt
  -> U17
set_u17_x = RIP.setUnionPayload

instance HasCField.HasCField U17 "u17_x" where

  type CFieldType U17 "u17_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u17_x" (RIP.Ptr U17) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u17_x")

{-| __C declaration:__ @union U18@

    __defined at:__ @attributes\/visibility\/types.h 89:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U18 = U18
  { unwrapU18 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U18

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U18

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U18

deriving via Marshal.EquivStorable U18 instance RIP.Storable U18

{-|

    __See:__ 'set_u18_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 89:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u18_x ::
     U18
  -> RIP.CInt
get_u18_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u18_x'

-}
set_u18_x ::
     RIP.CInt
  -> U18
set_u18_x = RIP.setUnionPayload

instance HasCField.HasCField U18 "u18_x" where

  type CFieldType U18 "u18_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u18_x" (RIP.Ptr U18) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u18_x")

{-| __C declaration:__ @union U19@

    __defined at:__ @attributes\/visibility\/types.h 90:50@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype U19 = U19
  { unwrapU19 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize U19

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw U19

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw U19

deriving via Marshal.EquivStorable U19 instance RIP.Storable U19

{-|

    __See:__ 'set_u19_x'

    __C declaration:__ @x@

    __defined at:__ @attributes\/visibility\/types.h 90:60@

    __exported by:__ @attributes\/visibility\/types.h@
-}
get_u19_x ::
     U19
  -> RIP.CInt
get_u19_x = RIP.getUnionPayload

{-|

    __See:__ 'get_u19_x'

-}
set_u19_x ::
     RIP.CInt
  -> U19
set_u19_x = RIP.setUnionPayload

instance HasCField.HasCField U19 "u19_x" where

  type CFieldType U19 "u19_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "u19_x" (RIP.Ptr U19) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"u19_x")

{-| __C declaration:__ @enum E0@

    __defined at:__ @attributes\/visibility\/types.h 93:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data E0

{-| __C declaration:__ @enum E1@

    __defined at:__ @attributes\/visibility\/types.h 94:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data E1

{-| __C declaration:__ @enum E2@

    __defined at:__ @attributes\/visibility\/types.h 95:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data E2

{-| __C declaration:__ @enum E3@

    __defined at:__ @attributes\/visibility\/types.h 96:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data E3

{-| __C declaration:__ @enum E4@

    __defined at:__ @attributes\/visibility\/types.h 97:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
data E4

{-| __C declaration:__ @enum E5@

    __defined at:__ @attributes\/visibility\/types.h 100:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E5 = E5
  { unwrapE5 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E5 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E5 where

  readRaw =
    \ptr0 ->
          pure E5
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E5 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E5 unwrapE52 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE52

deriving via Marshal.EquivStorable E5 instance RIP.Storable E5

deriving via RIP.CUInt instance RIP.Prim E5

instance CEnum.CEnum E5 where

  type CEnumZ E5 = RIP.CUInt

  toCEnum = E5

  fromCEnum = RIP.getField @"unwrapE5"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X5")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E5"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E5"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E5 where

  minDeclaredValue = X5

  maxDeclaredValue = X5

instance Show E5 where

  showsPrec = CEnum.shows

instance Read E5 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE5" (RIP.Ptr E5) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE5")

instance HasCField.HasCField E5 "unwrapE5" where

  type CFieldType E5 "unwrapE5" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x5@

    __defined at:__ @attributes\/visibility\/types.h 100:54@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X5 :: E5
pattern X5 = E5 0

{-| __C declaration:__ @enum E6@

    __defined at:__ @attributes\/visibility\/types.h 101:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E6 = E6
  { unwrapE6 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E6 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E6 where

  readRaw =
    \ptr0 ->
          pure E6
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E6 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E6 unwrapE62 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE62

deriving via Marshal.EquivStorable E6 instance RIP.Storable E6

deriving via RIP.CUInt instance RIP.Prim E6

instance CEnum.CEnum E6 where

  type CEnumZ E6 = RIP.CUInt

  toCEnum = E6

  fromCEnum = RIP.getField @"unwrapE6"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X6")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E6"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E6"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E6 where

  minDeclaredValue = X6

  maxDeclaredValue = X6

instance Show E6 where

  showsPrec = CEnum.shows

instance Read E6 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE6" (RIP.Ptr E6) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE6")

instance HasCField.HasCField E6 "unwrapE6" where

  type CFieldType E6 "unwrapE6" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x6@

    __defined at:__ @attributes\/visibility\/types.h 101:54@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X6 :: E6
pattern X6 = E6 0

{-| __C declaration:__ @enum E7@

    __defined at:__ @attributes\/visibility\/types.h 102:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E7 = E7
  { unwrapE7 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E7 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E7 where

  readRaw =
    \ptr0 ->
          pure E7
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E7 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E7 unwrapE72 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE72

deriving via Marshal.EquivStorable E7 instance RIP.Storable E7

deriving via RIP.CUInt instance RIP.Prim E7

instance CEnum.CEnum E7 where

  type CEnumZ E7 = RIP.CUInt

  toCEnum = E7

  fromCEnum = RIP.getField @"unwrapE7"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X7")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E7"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E7"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E7 where

  minDeclaredValue = X7

  maxDeclaredValue = X7

instance Show E7 where

  showsPrec = CEnum.shows

instance Read E7 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE7" (RIP.Ptr E7) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE7")

instance HasCField.HasCField E7 "unwrapE7" where

  type CFieldType E7 "unwrapE7" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x7@

    __defined at:__ @attributes\/visibility\/types.h 102:54@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X7 :: E7
pattern X7 = E7 0

{-| __C declaration:__ @enum E8@

    __defined at:__ @attributes\/visibility\/types.h 103:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E8 = E8
  { unwrapE8 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E8 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E8 where

  readRaw =
    \ptr0 ->
          pure E8
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E8 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E8 unwrapE82 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE82

deriving via Marshal.EquivStorable E8 instance RIP.Storable E8

deriving via RIP.CUInt instance RIP.Prim E8

instance CEnum.CEnum E8 where

  type CEnumZ E8 = RIP.CUInt

  toCEnum = E8

  fromCEnum = RIP.getField @"unwrapE8"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X8")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E8"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E8"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E8 where

  minDeclaredValue = X8

  maxDeclaredValue = X8

instance Show E8 where

  showsPrec = CEnum.shows

instance Read E8 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE8" (RIP.Ptr E8) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE8")

instance HasCField.HasCField E8 "unwrapE8" where

  type CFieldType E8 "unwrapE8" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x8@

    __defined at:__ @attributes\/visibility\/types.h 103:54@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X8 :: E8
pattern X8 = E8 0

{-| __C declaration:__ @enum E9@

    __defined at:__ @attributes\/visibility\/types.h 104:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E9 = E9
  { unwrapE9 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E9 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E9 where

  readRaw =
    \ptr0 ->
          pure E9
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E9 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E9 unwrapE92 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE92

deriving via Marshal.EquivStorable E9 instance RIP.Storable E9

deriving via RIP.CUInt instance RIP.Prim E9

instance CEnum.CEnum E9 where

  type CEnumZ E9 = RIP.CUInt

  toCEnum = E9

  fromCEnum = RIP.getField @"unwrapE9"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X9")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E9"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E9"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E9 where

  minDeclaredValue = X9

  maxDeclaredValue = X9

instance Show E9 where

  showsPrec = CEnum.shows

instance Read E9 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE9" (RIP.Ptr E9) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE9")

instance HasCField.HasCField E9 "unwrapE9" where

  type CFieldType E9 "unwrapE9" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x9@

    __defined at:__ @attributes\/visibility\/types.h 104:54@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X9 :: E9
pattern X9 = E9 0

{-| __C declaration:__ @enum E10@

    __defined at:__ @attributes\/visibility\/types.h 113:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E10 = E10
  { unwrapE10 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E10 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E10 where

  readRaw =
    \ptr0 ->
          pure E10
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E10 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E10 unwrapE102 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE102

deriving via Marshal.EquivStorable E10 instance RIP.Storable E10

deriving via RIP.CUInt instance RIP.Prim E10

instance CEnum.CEnum E10 where

  type CEnumZ E10 = RIP.CUInt

  toCEnum = E10

  fromCEnum = RIP.getField @"unwrapE10"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X10")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E10"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E10"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E10 where

  minDeclaredValue = X10

  maxDeclaredValue = X10

instance Show E10 where

  showsPrec = CEnum.shows

instance Read E10 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE10" (RIP.Ptr E10) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE10")

instance HasCField.HasCField E10 "unwrapE10" where

  type CFieldType E10 "unwrapE10" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x10@

    __defined at:__ @attributes\/visibility\/types.h 113:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X10 :: E10
pattern X10 = E10 0

{-| __C declaration:__ @enum E11@

    __defined at:__ @attributes\/visibility\/types.h 114:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E11 = E11
  { unwrapE11 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E11 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E11 where

  readRaw =
    \ptr0 ->
          pure E11
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E11 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E11 unwrapE112 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE112

deriving via Marshal.EquivStorable E11 instance RIP.Storable E11

deriving via RIP.CUInt instance RIP.Prim E11

instance CEnum.CEnum E11 where

  type CEnumZ E11 = RIP.CUInt

  toCEnum = E11

  fromCEnum = RIP.getField @"unwrapE11"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X11")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E11"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E11"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E11 where

  minDeclaredValue = X11

  maxDeclaredValue = X11

instance Show E11 where

  showsPrec = CEnum.shows

instance Read E11 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE11" (RIP.Ptr E11) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE11")

instance HasCField.HasCField E11 "unwrapE11" where

  type CFieldType E11 "unwrapE11" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x11@

    __defined at:__ @attributes\/visibility\/types.h 114:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X11 :: E11
pattern X11 = E11 0

{-| __C declaration:__ @enum E12@

    __defined at:__ @attributes\/visibility\/types.h 115:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E12 = E12
  { unwrapE12 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E12 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E12 where

  readRaw =
    \ptr0 ->
          pure E12
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E12 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E12 unwrapE122 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE122

deriving via Marshal.EquivStorable E12 instance RIP.Storable E12

deriving via RIP.CUInt instance RIP.Prim E12

instance CEnum.CEnum E12 where

  type CEnumZ E12 = RIP.CUInt

  toCEnum = E12

  fromCEnum = RIP.getField @"unwrapE12"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X12")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E12"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E12"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E12 where

  minDeclaredValue = X12

  maxDeclaredValue = X12

instance Show E12 where

  showsPrec = CEnum.shows

instance Read E12 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE12" (RIP.Ptr E12) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE12")

instance HasCField.HasCField E12 "unwrapE12" where

  type CFieldType E12 "unwrapE12" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x12@

    __defined at:__ @attributes\/visibility\/types.h 115:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X12 :: E12
pattern X12 = E12 0

{-| __C declaration:__ @enum E13@

    __defined at:__ @attributes\/visibility\/types.h 116:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E13 = E13
  { unwrapE13 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E13 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E13 where

  readRaw =
    \ptr0 ->
          pure E13
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E13 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E13 unwrapE132 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE132

deriving via Marshal.EquivStorable E13 instance RIP.Storable E13

deriving via RIP.CUInt instance RIP.Prim E13

instance CEnum.CEnum E13 where

  type CEnumZ E13 = RIP.CUInt

  toCEnum = E13

  fromCEnum = RIP.getField @"unwrapE13"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X13")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E13"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E13"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E13 where

  minDeclaredValue = X13

  maxDeclaredValue = X13

instance Show E13 where

  showsPrec = CEnum.shows

instance Read E13 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE13" (RIP.Ptr E13) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE13")

instance HasCField.HasCField E13 "unwrapE13" where

  type CFieldType E13 "unwrapE13" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x13@

    __defined at:__ @attributes\/visibility\/types.h 116:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X13 :: E13
pattern X13 = E13 0

{-| __C declaration:__ @enum E14@

    __defined at:__ @attributes\/visibility\/types.h 117:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E14 = E14
  { unwrapE14 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E14 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E14 where

  readRaw =
    \ptr0 ->
          pure E14
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E14 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E14 unwrapE142 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE142

deriving via Marshal.EquivStorable E14 instance RIP.Storable E14

deriving via RIP.CUInt instance RIP.Prim E14

instance CEnum.CEnum E14 where

  type CEnumZ E14 = RIP.CUInt

  toCEnum = E14

  fromCEnum = RIP.getField @"unwrapE14"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X14")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E14"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E14"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E14 where

  minDeclaredValue = X14

  maxDeclaredValue = X14

instance Show E14 where

  showsPrec = CEnum.shows

instance Read E14 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE14" (RIP.Ptr E14) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE14")

instance HasCField.HasCField E14 "unwrapE14" where

  type CFieldType E14 "unwrapE14" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x14@

    __defined at:__ @attributes\/visibility\/types.h 117:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X14 :: E14
pattern X14 = E14 0

{-| __C declaration:__ @enum E15@

    __defined at:__ @attributes\/visibility\/types.h 126:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E15 = E15
  { unwrapE15 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E15 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E15 where

  readRaw =
    \ptr0 ->
          pure E15
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E15 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E15 unwrapE152 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE152

deriving via Marshal.EquivStorable E15 instance RIP.Storable E15

deriving via RIP.CUInt instance RIP.Prim E15

instance CEnum.CEnum E15 where

  type CEnumZ E15 = RIP.CUInt

  toCEnum = E15

  fromCEnum = RIP.getField @"unwrapE15"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X15")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E15"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E15"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E15 where

  minDeclaredValue = X15

  maxDeclaredValue = X15

instance Show E15 where

  showsPrec = CEnum.shows

instance Read E15 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE15" (RIP.Ptr E15) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE15")

instance HasCField.HasCField E15 "unwrapE15" where

  type CFieldType E15 "unwrapE15" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x15@

    __defined at:__ @attributes\/visibility\/types.h 126:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X15 :: E15
pattern X15 = E15 0

{-| __C declaration:__ @enum E16@

    __defined at:__ @attributes\/visibility\/types.h 127:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E16 = E16
  { unwrapE16 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E16 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E16 where

  readRaw =
    \ptr0 ->
          pure E16
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E16 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E16 unwrapE162 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE162

deriving via Marshal.EquivStorable E16 instance RIP.Storable E16

deriving via RIP.CUInt instance RIP.Prim E16

instance CEnum.CEnum E16 where

  type CEnumZ E16 = RIP.CUInt

  toCEnum = E16

  fromCEnum = RIP.getField @"unwrapE16"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X16")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E16"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E16"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E16 where

  minDeclaredValue = X16

  maxDeclaredValue = X16

instance Show E16 where

  showsPrec = CEnum.shows

instance Read E16 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE16" (RIP.Ptr E16) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE16")

instance HasCField.HasCField E16 "unwrapE16" where

  type CFieldType E16 "unwrapE16" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x16@

    __defined at:__ @attributes\/visibility\/types.h 127:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X16 :: E16
pattern X16 = E16 0

{-| __C declaration:__ @enum E17@

    __defined at:__ @attributes\/visibility\/types.h 128:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E17 = E17
  { unwrapE17 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E17 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E17 where

  readRaw =
    \ptr0 ->
          pure E17
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E17 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E17 unwrapE172 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE172

deriving via Marshal.EquivStorable E17 instance RIP.Storable E17

deriving via RIP.CUInt instance RIP.Prim E17

instance CEnum.CEnum E17 where

  type CEnumZ E17 = RIP.CUInt

  toCEnum = E17

  fromCEnum = RIP.getField @"unwrapE17"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X17")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E17"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E17"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E17 where

  minDeclaredValue = X17

  maxDeclaredValue = X17

instance Show E17 where

  showsPrec = CEnum.shows

instance Read E17 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE17" (RIP.Ptr E17) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE17")

instance HasCField.HasCField E17 "unwrapE17" where

  type CFieldType E17 "unwrapE17" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x17@

    __defined at:__ @attributes\/visibility\/types.h 128:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X17 :: E17
pattern X17 = E17 0

{-| __C declaration:__ @enum E18@

    __defined at:__ @attributes\/visibility\/types.h 129:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E18 = E18
  { unwrapE18 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E18 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E18 where

  readRaw =
    \ptr0 ->
          pure E18
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E18 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E18 unwrapE182 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE182

deriving via Marshal.EquivStorable E18 instance RIP.Storable E18

deriving via RIP.CUInt instance RIP.Prim E18

instance CEnum.CEnum E18 where

  type CEnumZ E18 = RIP.CUInt

  toCEnum = E18

  fromCEnum = RIP.getField @"unwrapE18"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X18")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E18"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E18"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E18 where

  minDeclaredValue = X18

  maxDeclaredValue = X18

instance Show E18 where

  showsPrec = CEnum.shows

instance Read E18 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE18" (RIP.Ptr E18) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE18")

instance HasCField.HasCField E18 "unwrapE18" where

  type CFieldType E18 "unwrapE18" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x18@

    __defined at:__ @attributes\/visibility\/types.h 129:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X18 :: E18
pattern X18 = E18 0

{-| __C declaration:__ @enum E19@

    __defined at:__ @attributes\/visibility\/types.h 130:49@

    __exported by:__ @attributes\/visibility\/types.h@
-}
newtype E19 = E19
  { unwrapE19 :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E19 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E19 where

  readRaw =
    \ptr0 ->
          pure E19
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E19 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E19 unwrapE192 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapE192

deriving via Marshal.EquivStorable E19 instance RIP.Storable E19

deriving via RIP.CUInt instance RIP.Prim E19

instance CEnum.CEnum E19 where

  type CEnumZ E19 = RIP.CUInt

  toCEnum = E19

  fromCEnum = RIP.getField @"unwrapE19"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X19")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E19"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E19"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E19 where

  minDeclaredValue = X19

  maxDeclaredValue = X19

instance Show E19 where

  showsPrec = CEnum.shows

instance Read E19 where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ty ~ RIP.CUInt
         ) => RIP.HasField "unwrapE19" (RIP.Ptr E19) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE19")

instance HasCField.HasCField E19 "unwrapE19" where

  type CFieldType E19 "unwrapE19" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x19@

    __defined at:__ @attributes\/visibility\/types.h 130:55@

    __exported by:__ @attributes\/visibility\/types.h@
-}
pattern X19 :: E19
pattern X19 = E19 0
