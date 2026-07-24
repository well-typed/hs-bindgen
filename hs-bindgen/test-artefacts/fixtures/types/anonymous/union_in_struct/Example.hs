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
    ( Example.Outer1_anon'fieldX(..)
    , Example.Outer1(..)
    , Example.Outer2_fieldB(..)
    , Example.Outer2(..)
    , Example.Inner3(..)
    , Example.Outer3(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union \@outer1_anon\'fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 8:3@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
newtype Outer1_anon'fieldX = Outer1_anon'fieldX
  { unwrapOuter1_anon'fieldX :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Outer1_anon'fieldX

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Outer1_anon'fieldX

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Outer1_anon'fieldX

deriving via Marshal.EquivStorable Outer1_anon'fieldX instance BG.Storable Outer1_anon'fieldX

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Outer1_anon'fieldX

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 9:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_anon'fieldX_fieldX" Outer1_anon'fieldX ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 9:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer1_anon'fieldX_fieldX" Outer1_anon'fieldX ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"outer1_anon'fieldX_fieldX" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_anon'fieldX_fieldX" (BG.Ptr Outer1_anon'fieldX) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_anon'fieldX_fieldX")

instance HasCField.HasCField Outer1_anon'fieldX "outer1_anon'fieldX_fieldX" where

  type CFieldType Outer1_anon'fieldX "outer1_anon'fieldX_fieldX" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 10:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_anon'fieldX_fieldY" Outer1_anon'fieldX ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 10:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer1_anon'fieldX_fieldY" Outer1_anon'fieldX ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"outer1_anon'fieldX_fieldY" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_anon'fieldX_fieldY" (BG.Ptr Outer1_anon'fieldX) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_anon'fieldX_fieldY")

instance HasCField.HasCField Outer1_anon'fieldX "outer1_anon'fieldX_fieldY" where

  type CFieldType Outer1_anon'fieldX "outer1_anon'fieldX_fieldY" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct outer1@

    __defined at:__ @types\/anonymous\/union_in_struct.h 6:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
data Outer1 = Outer1
  { outer1_fieldA :: BG.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/union_in_struct.h 7:8@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer1_anon'fieldX :: Outer1_anon'fieldX
    {- ^ __C declaration:__ @anon\'fieldX@

         __defined at:__ @types\/anonymous\/union_in_struct.h 8:3@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer1_fieldC :: BG.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/union_in_struct.h 12:7@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize Outer1 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer1 where

  readRaw =
    \ptr0 ->
          pure Outer1
      <*> HasCField.readRaw (BG.Proxy @"outer1_fieldA") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer1_anon'fieldX") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer1_fieldC") ptr0

instance Marshal.WriteRaw Outer1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer1 outer1_fieldA2 outer1_anon'fieldX3 outer1_fieldC4 ->
               HasCField.writeRaw (BG.Proxy @"outer1_fieldA") ptr0 outer1_fieldA2
            >> HasCField.writeRaw (BG.Proxy @"outer1_anon'fieldX") ptr0 outer1_anon'fieldX3
            >> HasCField.writeRaw (BG.Proxy @"outer1_fieldC") ptr0 outer1_fieldC4

deriving via Marshal.EquivStorable Outer1 instance BG.Storable Outer1

deriving via Struct.IsStructViaStorable Outer1 instance Struct.IsStruct Outer1

{-| __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/union_in_struct.h 7:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "outer1_fieldA" Outer1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer1 { outer1_fieldA = y1
                 , outer1_anon'fieldX = BG.getField @"outer1_anon'fieldX" x0
                 , outer1_fieldC = BG.getField @"outer1_fieldC" x0
                 }
      , BG.getField @"outer1_fieldA" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "outer1_fieldA" (BG.Ptr Outer1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_fieldA")

instance HasCField.HasCField Outer1 "outer1_fieldA" where

  type CFieldType Outer1 "outer1_fieldA" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @anon\'fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 8:3@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ Outer1_anon'fieldX
         ) => BG.CompatHasField.HasField "outer1_anon'fieldX" Outer1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer1 { outer1_anon'fieldX = y1
                 , outer1_fieldA = BG.getField @"outer1_fieldA" x0
                 , outer1_fieldC = BG.getField @"outer1_fieldC" x0
                 }
      , BG.getField @"outer1_anon'fieldX" x0
      )

instance ( ty ~ Outer1_anon'fieldX
         ) => BG.HasField "outer1_anon'fieldX" (BG.Ptr Outer1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_anon'fieldX")

instance HasCField.HasCField Outer1 "outer1_anon'fieldX" where

  type CFieldType Outer1 "outer1_anon'fieldX" =
    Outer1_anon'fieldX

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 9:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "outer1_fieldX" Outer1 ty where

  getField =
    \x0 ->
      BG.getField @"outer1_anon'fieldX_fieldX" (BG.getField @"outer1_anon'fieldX" x0)

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 9:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer1_fieldX" Outer1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"outer1_anon'fieldX" x0 (\z2 ->
                                                                    BG.CompatHasField.setField @"outer1_anon'fieldX_fieldX" z2 y1)
      , BG.getField @"outer1_fieldX" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_fieldX" (BG.Ptr Outer1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_fieldX")

instance HasCField.HasCField Outer1 "outer1_fieldX" where

  type CFieldType Outer1 "outer1_fieldX" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 10:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "outer1_fieldY" Outer1 ty where

  getField =
    \x0 ->
      BG.getField @"outer1_anon'fieldX_fieldY" (BG.getField @"outer1_anon'fieldX" x0)

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 10:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer1_fieldY" Outer1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          BG.CompatHasField.modifyField @"outer1_anon'fieldX" x0 (\z2 ->
                                                                    BG.CompatHasField.setField @"outer1_anon'fieldX_fieldY" z2 y1)
      , BG.getField @"outer1_fieldY" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_fieldY" (BG.Ptr Outer1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_fieldY")

instance HasCField.HasCField Outer1 "outer1_fieldY" where

  type CFieldType Outer1 "outer1_fieldY" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/union_in_struct.h 12:7@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer1_fieldC" Outer1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer1 { outer1_fieldC = y1
                 , outer1_fieldA = BG.getField @"outer1_fieldA" x0
                 , outer1_anon'fieldX = BG.getField @"outer1_anon'fieldX" x0
                 }
      , BG.getField @"outer1_fieldC" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer1_fieldC" (BG.Ptr Outer1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer1_fieldC")

instance HasCField.HasCField Outer1 "outer1_fieldC" where

  type CFieldType Outer1 "outer1_fieldC" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @union \@outer2_fieldB@

    __defined at:__ @types\/anonymous\/union_in_struct.h 17:3@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
newtype Outer2_fieldB = Outer2_fieldB
  { unwrapOuter2_fieldB :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Outer2_fieldB

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Outer2_fieldB

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Outer2_fieldB

deriving via Marshal.EquivStorable Outer2_fieldB instance BG.Storable Outer2_fieldB

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Outer2_fieldB

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 18:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "outer2_fieldB_fieldX" Outer2_fieldB ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 18:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer2_fieldB_fieldX" Outer2_fieldB ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"outer2_fieldB_fieldX" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer2_fieldB_fieldX" (BG.Ptr Outer2_fieldB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer2_fieldB_fieldX")

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldX" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldX" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 19:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.HasField "outer2_fieldB_fieldY" Outer2_fieldB ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 19:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer2_fieldB_fieldY" Outer2_fieldB ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"outer2_fieldB_fieldY" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer2_fieldB_fieldY" (BG.Ptr Outer2_fieldB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer2_fieldB_fieldY")

instance HasCField.HasCField Outer2_fieldB "outer2_fieldB_fieldY" where

  type CFieldType Outer2_fieldB "outer2_fieldB_fieldY" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct outer2@

    __defined at:__ @types\/anonymous\/union_in_struct.h 15:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
data Outer2 = Outer2
  { outer2_fieldA :: BG.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/union_in_struct.h 16:8@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer2_fieldB :: Outer2_fieldB
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @types\/anonymous\/union_in_struct.h 20:5@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer2_fieldC :: BG.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/union_in_struct.h 21:7@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize Outer2 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer2 where

  readRaw =
    \ptr0 ->
          pure Outer2
      <*> HasCField.readRaw (BG.Proxy @"outer2_fieldA") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer2_fieldB") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer2_fieldC") ptr0

instance Marshal.WriteRaw Outer2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer2 outer2_fieldA2 outer2_fieldB3 outer2_fieldC4 ->
               HasCField.writeRaw (BG.Proxy @"outer2_fieldA") ptr0 outer2_fieldA2
            >> HasCField.writeRaw (BG.Proxy @"outer2_fieldB") ptr0 outer2_fieldB3
            >> HasCField.writeRaw (BG.Proxy @"outer2_fieldC") ptr0 outer2_fieldC4

deriving via Marshal.EquivStorable Outer2 instance BG.Storable Outer2

deriving via Struct.IsStructViaStorable Outer2 instance Struct.IsStruct Outer2

{-| __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/union_in_struct.h 16:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "outer2_fieldA" Outer2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer2 { outer2_fieldA = y1
                 , outer2_fieldB = BG.getField @"outer2_fieldB" x0
                 , outer2_fieldC = BG.getField @"outer2_fieldC" x0
                 }
      , BG.getField @"outer2_fieldA" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "outer2_fieldA" (BG.Ptr Outer2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer2_fieldA")

instance HasCField.HasCField Outer2 "outer2_fieldA" where

  type CFieldType Outer2 "outer2_fieldA" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fieldB@

    __defined at:__ @types\/anonymous\/union_in_struct.h 20:5@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ Outer2_fieldB
         ) => BG.CompatHasField.HasField "outer2_fieldB" Outer2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer2 { outer2_fieldB = y1
                 , outer2_fieldA = BG.getField @"outer2_fieldA" x0
                 , outer2_fieldC = BG.getField @"outer2_fieldC" x0
                 }
      , BG.getField @"outer2_fieldB" x0
      )

instance ( ty ~ Outer2_fieldB
         ) => BG.HasField "outer2_fieldB" (BG.Ptr Outer2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer2_fieldB")

instance HasCField.HasCField Outer2 "outer2_fieldB" where

  type CFieldType Outer2 "outer2_fieldB" =
    Outer2_fieldB

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/union_in_struct.h 21:7@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer2_fieldC" Outer2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer2 { outer2_fieldC = y1
                 , outer2_fieldA = BG.getField @"outer2_fieldA" x0
                 , outer2_fieldB = BG.getField @"outer2_fieldB" x0
                 }
      , BG.getField @"outer2_fieldC" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer2_fieldC" (BG.Ptr Outer2) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer2_fieldC")

instance HasCField.HasCField Outer2 "outer2_fieldC" where

  type CFieldType Outer2 "outer2_fieldC" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @union inner3@

    __defined at:__ @types\/anonymous\/union_in_struct.h 26:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
newtype Inner3 = Inner3
  { unwrapInner3 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Inner3

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Inner3

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Inner3

deriving via Marshal.EquivStorable Inner3 instance BG.Storable Inner3

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Inner3

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 27:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "inner3_fieldX" Inner3 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldX@

    __defined at:__ @types\/anonymous\/union_in_struct.h 27:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "inner3_fieldX" Inner3 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"inner3_fieldX" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "inner3_fieldX" (BG.Ptr Inner3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"inner3_fieldX")

instance HasCField.HasCField Inner3 "inner3_fieldX" where

  type CFieldType Inner3 "inner3_fieldX" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 28:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "inner3_fieldY" Inner3 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @fieldY@

    __defined at:__ @types\/anonymous\/union_in_struct.h 28:9@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "inner3_fieldY" Inner3 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"inner3_fieldY" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "inner3_fieldY" (BG.Ptr Inner3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"inner3_fieldY")

instance HasCField.HasCField Inner3 "inner3_fieldY" where

  type CFieldType Inner3 "inner3_fieldY" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct outer3@

    __defined at:__ @types\/anonymous\/union_in_struct.h 24:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
data Outer3 = Outer3
  { outer3_fieldA :: BG.CChar
    {- ^ __C declaration:__ @fieldA@

         __defined at:__ @types\/anonymous\/union_in_struct.h 25:8@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer3_fieldB :: Inner3
    {- ^ __C declaration:__ @fieldB@

         __defined at:__ @types\/anonymous\/union_in_struct.h 29:5@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  , outer3_fieldC :: BG.CInt
    {- ^ __C declaration:__ @fieldC@

         __defined at:__ @types\/anonymous\/union_in_struct.h 30:7@

         __exported by:__ @types\/anonymous\/union_in_struct.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize Outer3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Outer3 where

  readRaw =
    \ptr0 ->
          pure Outer3
      <*> HasCField.readRaw (BG.Proxy @"outer3_fieldA") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer3_fieldB") ptr0
      <*> HasCField.readRaw (BG.Proxy @"outer3_fieldC") ptr0

instance Marshal.WriteRaw Outer3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Outer3 outer3_fieldA2 outer3_fieldB3 outer3_fieldC4 ->
               HasCField.writeRaw (BG.Proxy @"outer3_fieldA") ptr0 outer3_fieldA2
            >> HasCField.writeRaw (BG.Proxy @"outer3_fieldB") ptr0 outer3_fieldB3
            >> HasCField.writeRaw (BG.Proxy @"outer3_fieldC") ptr0 outer3_fieldC4

deriving via Marshal.EquivStorable Outer3 instance BG.Storable Outer3

deriving via Struct.IsStructViaStorable Outer3 instance Struct.IsStruct Outer3

{-| __C declaration:__ @fieldA@

    __defined at:__ @types\/anonymous\/union_in_struct.h 25:8@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "outer3_fieldA" Outer3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer3 { outer3_fieldA = y1
                 , outer3_fieldB = BG.getField @"outer3_fieldB" x0
                 , outer3_fieldC = BG.getField @"outer3_fieldC" x0
                 }
      , BG.getField @"outer3_fieldA" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "outer3_fieldA" (BG.Ptr Outer3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer3_fieldA")

instance HasCField.HasCField Outer3 "outer3_fieldA" where

  type CFieldType Outer3 "outer3_fieldA" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @fieldB@

    __defined at:__ @types\/anonymous\/union_in_struct.h 29:5@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ Inner3
         ) => BG.CompatHasField.HasField "outer3_fieldB" Outer3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer3 { outer3_fieldB = y1
                 , outer3_fieldA = BG.getField @"outer3_fieldA" x0
                 , outer3_fieldC = BG.getField @"outer3_fieldC" x0
                 }
      , BG.getField @"outer3_fieldB" x0
      )

instance ( ty ~ Inner3
         ) => BG.HasField "outer3_fieldB" (BG.Ptr Outer3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer3_fieldB")

instance HasCField.HasCField Outer3 "outer3_fieldB" where

  type CFieldType Outer3 "outer3_fieldB" = Inner3

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @fieldC@

    __defined at:__ @types\/anonymous\/union_in_struct.h 30:7@

    __exported by:__ @types\/anonymous\/union_in_struct.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "outer3_fieldC" Outer3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Outer3 { outer3_fieldC = y1
                 , outer3_fieldA = BG.getField @"outer3_fieldA" x0
                 , outer3_fieldB = BG.getField @"outer3_fieldB" x0
                 }
      , BG.getField @"outer3_fieldC" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "outer3_fieldC" (BG.Ptr Outer3) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"outer3_fieldC")

instance HasCField.HasCField Outer3 "outer3_fieldC" where

  type CFieldType Outer3 "outer3_fieldC" = BG.CInt

  offset# = \_ -> \_ -> 8
