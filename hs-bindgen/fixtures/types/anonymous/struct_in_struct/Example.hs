{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Outer1_fieldX(..)
    , Example.Outer1(..)
    , Example.Outer2_fieldB(..)
    , Example.Outer2(..)
    , Example.Inner3(..)
    , Example.Outer3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@outer1_fieldX@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 8:3@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Outer1_fieldX = Outer1_fieldX
  { outer1_fieldX_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 9:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer1_fieldX_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 10:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer1_fieldX where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer1_fieldX where

  readRaw =
    \ptr0 ->
          pure Outer1_fieldX
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldX_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldX_fieldY") ptr0

instance Marshal.WriteRaw Outer1_fieldX where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer1_fieldX outer1_fieldX_fieldX2 outer1_fieldX_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"outer1_fieldX_fieldX") ptr0 outer1_fieldX_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"outer1_fieldX_fieldY") ptr0 outer1_fieldX_fieldY3

deriving via Marshal.EquivStorable Outer1_fieldX instance RIP.Storable Outer1_fieldX

instance HasCField.HasCField Outer1_fieldX "outer1_fieldX_fieldX" where

  type CFieldType Outer1_fieldX "outer1_fieldX_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer1_fieldX_fieldX" (RIP.Ptr Outer1_fieldX) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX_fieldX")

instance HasCField.HasCField Outer1_fieldX "outer1_fieldX_fieldY" where

  type CFieldType Outer1_fieldX "outer1_fieldX_fieldY" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer1_fieldX_fieldY" (RIP.Ptr Outer1_fieldX) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX_fieldY")

{-| __C declaration:__ @struct outer1@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 6:8@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Outer1 = Outer1
  { outer1_fieldA :: RIP.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 7:8@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer1_fieldX :: Outer1_fieldX
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 8:3@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer1_fieldC :: RIP.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 12:7@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer1 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer1 where

  readRaw =
    \ptr0 ->
          pure Outer1
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer1_fieldC") ptr0

instance Marshal.WriteRaw Outer1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer1 outer1_fieldA2 outer1_fieldX3 outer1_fieldC4 ->
               HasCField.writeRaw (RIP.Proxy @"outer1_fieldA") ptr0 outer1_fieldA2
            >> HasCField.writeRaw (RIP.Proxy @"outer1_fieldX") ptr0 outer1_fieldX3
            >> HasCField.writeRaw (RIP.Proxy @"outer1_fieldC") ptr0 outer1_fieldC4

deriving via Marshal.EquivStorable Outer1 instance RIP.Storable Outer1

instance HasCField.HasCField Outer1 "outer1_fieldA" where

  type CFieldType Outer1 "outer1_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CChar
         ) => RIP.HasField "outer1_fieldA" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldA")

instance HasCField.HasCField Outer1 "outer1_fieldX" where

  type CFieldType Outer1 "outer1_fieldX" =
    Outer1_fieldX

  offset# = \_ -> \_ -> 4

instance ( (~) ty Outer1_fieldX
         ) => RIP.HasField "outer1_fieldX" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldX")

instance HasCField.HasCField Outer1 "outer1_fieldC" where

  type CFieldType Outer1 "outer1_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer1_fieldC" (RIP.Ptr Outer1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer1_fieldC")

{-| __C declaration:__ @struct \@outer2_fieldB@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 17:3@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Outer2_fieldB = Outer2_fieldB
  { outer2_fieldB_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 18:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer2_fieldB_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 19:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer2_fieldB where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer2_fieldB where

  readRaw =
    \ptr0 ->
          pure Outer2_fieldB
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldB_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldB_fieldY") ptr0

instance Marshal.WriteRaw Outer2_fieldB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer2_fieldB outer2_fieldB_fieldX2 outer2_fieldB_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"outer2_fieldB_fieldX") ptr0 outer2_fieldB_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"outer2_fieldB_fieldY") ptr0 outer2_fieldB_fieldY3

deriving via Marshal.EquivStorable Outer2_fieldB instance RIP.Storable Outer2_fieldB

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldX" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldX" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer2_fieldB_fieldX" (RIP.Ptr Outer2_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB_fieldX")

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldY" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldY" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer2_fieldB_fieldY" (RIP.Ptr Outer2_fieldB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB_fieldY")

{-| __C declaration:__ @struct outer2@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 15:8@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Outer2 = Outer2
  { outer2_fieldA :: RIP.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 16:8@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer2_fieldB :: Outer2_fieldB
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 20:5@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer2_fieldC :: RIP.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 21:7@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer2 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer2 where

  readRaw =
    \ptr0 ->
          pure Outer2
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldB") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer2_fieldC") ptr0

instance Marshal.WriteRaw Outer2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer2 outer2_fieldA2 outer2_fieldB3 outer2_fieldC4 ->
               HasCField.writeRaw (RIP.Proxy @"outer2_fieldA") ptr0 outer2_fieldA2
            >> HasCField.writeRaw (RIP.Proxy @"outer2_fieldB") ptr0 outer2_fieldB3
            >> HasCField.writeRaw (RIP.Proxy @"outer2_fieldC") ptr0 outer2_fieldC4

deriving via Marshal.EquivStorable Outer2 instance RIP.Storable Outer2

instance HasCField.HasCField Outer2 "outer2_fieldA" where

  type CFieldType Outer2 "outer2_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CChar
         ) => RIP.HasField "outer2_fieldA" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldA")

instance HasCField.HasCField Outer2 "outer2_fieldB" where

  type CFieldType Outer2 "outer2_fieldB" =
    Outer2_fieldB

  offset# = \_ -> \_ -> 4

instance ( (~) ty Outer2_fieldB
         ) => RIP.HasField "outer2_fieldB" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldB")

instance HasCField.HasCField Outer2 "outer2_fieldC" where

  type CFieldType Outer2 "outer2_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer2_fieldC" (RIP.Ptr Outer2) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer2_fieldC")

{-| __C declaration:__ @struct inner3@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 26:10@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Inner3 = Inner3
  { inner3_fieldX :: RIP.CInt
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 27:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , inner3_fieldY :: RIP.CInt
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 28:9@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Inner3 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Inner3 where

  readRaw =
    \ptr0 ->
          pure Inner3
      <*> HasCField.readRaw (RIP.Proxy @"inner3_fieldX") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"inner3_fieldY") ptr0

instance Marshal.WriteRaw Inner3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inner3 inner3_fieldX2 inner3_fieldY3 ->
               HasCField.writeRaw (RIP.Proxy @"inner3_fieldX") ptr0 inner3_fieldX2
            >> HasCField.writeRaw (RIP.Proxy @"inner3_fieldY") ptr0 inner3_fieldY3

deriving via Marshal.EquivStorable Inner3 instance RIP.Storable Inner3

instance HasCField.HasCField Inner3 "inner3_fieldX" where

  type CFieldType Inner3 "inner3_fieldX" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "inner3_fieldX" (RIP.Ptr Inner3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inner3_fieldX")

instance HasCField.HasCField Inner3 "inner3_fieldY" where

  type CFieldType Inner3 "inner3_fieldY" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "inner3_fieldY" (RIP.Ptr Inner3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"inner3_fieldY")

{-| __C declaration:__ @struct outer3@

    __defined at:__ @types\/anonymous\/struct_in_struct.h 24:8@

    __exported by:__ @types\/anonymous\/struct_in_struct.h@
-}
data Outer3 = Outer3
  { outer3_fieldA :: RIP.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 25:8@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer3_fieldB :: Inner3
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 29:5@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  , outer3_fieldC :: RIP.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/struct_in_struct.h 30:7@

         __exported by:__ @types\/anonymous\/struct_in_struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Outer3 where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer3 where

  readRaw =
    \ptr0 ->
          pure Outer3
      <*> HasCField.readRaw (RIP.Proxy @"outer3_fieldA") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer3_fieldB") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"outer3_fieldC") ptr0

instance Marshal.WriteRaw Outer3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer3 outer3_fieldA2 outer3_fieldB3 outer3_fieldC4 ->
               HasCField.writeRaw (RIP.Proxy @"outer3_fieldA") ptr0 outer3_fieldA2
            >> HasCField.writeRaw (RIP.Proxy @"outer3_fieldB") ptr0 outer3_fieldB3
            >> HasCField.writeRaw (RIP.Proxy @"outer3_fieldC") ptr0 outer3_fieldC4

deriving via Marshal.EquivStorable Outer3 instance RIP.Storable Outer3

instance HasCField.HasCField Outer3 "outer3_fieldA" where

  type CFieldType Outer3 "outer3_fieldA" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CChar
         ) => RIP.HasField "outer3_fieldA" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldA")

instance HasCField.HasCField Outer3 "outer3_fieldB" where

  type CFieldType Outer3 "outer3_fieldB" = Inner3

  offset# = \_ -> \_ -> 4

instance ( (~) ty Inner3
         ) => RIP.HasField "outer3_fieldB" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldB")

instance HasCField.HasCField Outer3 "outer3_fieldC" where

  type CFieldType Outer3 "outer3_fieldC" = RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "outer3_fieldC" (RIP.Ptr Outer3) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"outer3_fieldC")
