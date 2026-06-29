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
    ( Example.Has_implicit_fields_anon'x2_1(..)
    , Example.Has_implicit_fields_anon'x4_1(..)
    , Example.Has_implicit_fields_anon'x5_1(..)
    , Example.Has_implicit_fields(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Internal.Prelude.CompatHasField as RIP.CompatHasField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct \@has_implicit_fields_anon\'x2_1@

    __defined at:__ @types\/anonymous\/struct.h 7:3@

    __exported by:__ @types\/anonymous\/struct.h@
-}
data Has_implicit_fields_anon'x2_1 = Has_implicit_fields_anon'x2_1
  { has_implicit_fields_anon'x2_1_x2_1 :: RIP.CInt
    {- ^ __C declaration:__ @x2_1@

         __defined at:__ @types\/anonymous\/struct.h 8:9@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_anon'x2_1_x2_2 :: RIP.CInt
    {- ^ __C declaration:__ @x2_2@

         __defined at:__ @types\/anonymous\/struct.h 9:9@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Has_implicit_fields_anon'x2_1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Has_implicit_fields_anon'x2_1 where

  readRaw =
    \ptr0 ->
          pure Has_implicit_fields_anon'x2_1
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_2") ptr0

instance Marshal.WriteRaw Has_implicit_fields_anon'x2_1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Has_implicit_fields_anon'x2_1
            has_implicit_fields_anon'x2_1_x2_12
            has_implicit_fields_anon'x2_1_x2_23 ->
                 HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_1") ptr0 has_implicit_fields_anon'x2_1_x2_12
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_2") ptr0 has_implicit_fields_anon'x2_1_x2_23

deriving via Marshal.EquivStorable Has_implicit_fields_anon'x2_1 instance RIP.Storable Has_implicit_fields_anon'x2_1

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x2_1_x2_1" Has_implicit_fields_anon'x2_1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields_anon'x2_1 { has_implicit_fields_anon'x2_1_x2_1 = y1
                                        , has_implicit_fields_anon'x2_1_x2_2 = RIP.getField @"has_implicit_fields_anon'x2_1_x2_2" x0
                                        }
      , RIP.getField @"has_implicit_fields_anon'x2_1_x2_1" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x2_1_x2_1" (RIP.Ptr Has_implicit_fields_anon'x2_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_1")

instance HasCField.HasCField Has_implicit_fields_anon'x2_1 "has_implicit_fields_anon'x2_1_x2_1" where

  type CFieldType Has_implicit_fields_anon'x2_1 "has_implicit_fields_anon'x2_1_x2_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x2_1_x2_2" Has_implicit_fields_anon'x2_1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields_anon'x2_1 { has_implicit_fields_anon'x2_1_x2_2 = y1
                                        , has_implicit_fields_anon'x2_1_x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1_x2_1" x0
                                        }
      , RIP.getField @"has_implicit_fields_anon'x2_1_x2_2" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x2_1_x2_2" (RIP.Ptr Has_implicit_fields_anon'x2_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x2_1_x2_2")

instance HasCField.HasCField Has_implicit_fields_anon'x2_1 "has_implicit_fields_anon'x2_1_x2_2" where

  type CFieldType Has_implicit_fields_anon'x2_1 "has_implicit_fields_anon'x2_1_x2_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@has_implicit_fields_anon\'x4_1@

    __defined at:__ @types\/anonymous\/struct.h 12:3@

    __exported by:__ @types\/anonymous\/struct.h@
-}
data Has_implicit_fields_anon'x4_1 = Has_implicit_fields_anon'x4_1
  { has_implicit_fields_anon'x4_1_x4_1 :: RIP.CInt
    {- ^ __C declaration:__ @x4_1@

         __defined at:__ @types\/anonymous\/struct.h 13:9@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_anon'x4_1_x4_2 :: RIP.CInt
    {- ^ __C declaration:__ @x4_2@

         __defined at:__ @types\/anonymous\/struct.h 14:9@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Has_implicit_fields_anon'x4_1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Has_implicit_fields_anon'x4_1 where

  readRaw =
    \ptr0 ->
          pure Has_implicit_fields_anon'x4_1
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_2") ptr0

instance Marshal.WriteRaw Has_implicit_fields_anon'x4_1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Has_implicit_fields_anon'x4_1
            has_implicit_fields_anon'x4_1_x4_12
            has_implicit_fields_anon'x4_1_x4_23 ->
                 HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_1") ptr0 has_implicit_fields_anon'x4_1_x4_12
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_2") ptr0 has_implicit_fields_anon'x4_1_x4_23

deriving via Marshal.EquivStorable Has_implicit_fields_anon'x4_1 instance RIP.Storable Has_implicit_fields_anon'x4_1

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x4_1_x4_1" Has_implicit_fields_anon'x4_1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields_anon'x4_1 { has_implicit_fields_anon'x4_1_x4_1 = y1
                                        , has_implicit_fields_anon'x4_1_x4_2 = RIP.getField @"has_implicit_fields_anon'x4_1_x4_2" x0
                                        }
      , RIP.getField @"has_implicit_fields_anon'x4_1_x4_1" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x4_1_x4_1" (RIP.Ptr Has_implicit_fields_anon'x4_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_1")

instance HasCField.HasCField Has_implicit_fields_anon'x4_1 "has_implicit_fields_anon'x4_1_x4_1" where

  type CFieldType Has_implicit_fields_anon'x4_1 "has_implicit_fields_anon'x4_1_x4_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x4_1_x4_2" Has_implicit_fields_anon'x4_1 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields_anon'x4_1 { has_implicit_fields_anon'x4_1_x4_2 = y1
                                        , has_implicit_fields_anon'x4_1_x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1_x4_1" x0
                                        }
      , RIP.getField @"has_implicit_fields_anon'x4_1_x4_2" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x4_1_x4_2" (RIP.Ptr Has_implicit_fields_anon'x4_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x4_1_x4_2")

instance HasCField.HasCField Has_implicit_fields_anon'x4_1 "has_implicit_fields_anon'x4_1_x4_2" where

  type CFieldType Has_implicit_fields_anon'x4_1 "has_implicit_fields_anon'x4_1_x4_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @union \@has_implicit_fields_anon\'x5_1@

    __defined at:__ @types\/anonymous\/struct.h 16:3@

    __exported by:__ @types\/anonymous\/struct.h@
-}
newtype Has_implicit_fields_anon'x5_1 = Has_implicit_fields_anon'x5_1
  { unwrapHas_implicit_fields_anon'x5_1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize Has_implicit_fields_anon'x5_1

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw Has_implicit_fields_anon'x5_1

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw Has_implicit_fields_anon'x5_1

deriving via Marshal.EquivStorable Has_implicit_fields_anon'x5_1 instance RIP.Storable Has_implicit_fields_anon'x5_1

deriving via RIP.SizedByteArray 4 4 instance Union.IsUnion Has_implicit_fields_anon'x5_1

{-| __C declaration:__ @x5_1@

    __defined at:__ @types\/anonymous\/struct.h 17:9@

    __exported by:__ @types\/anonymous\/struct.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x5_1_x5_1" Has_implicit_fields_anon'x5_1 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x5_1@

    __defined at:__ @types\/anonymous\/struct.h 17:9@

    __exported by:__ @types\/anonymous\/struct.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x5_1_x5_1" Has_implicit_fields_anon'x5_1 ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"has_implicit_fields_anon'x5_1_x5_1" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x5_1_x5_1" (RIP.Ptr Has_implicit_fields_anon'x5_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x5_1_x5_1")

instance HasCField.HasCField Has_implicit_fields_anon'x5_1 "has_implicit_fields_anon'x5_1_x5_1" where

  type CFieldType Has_implicit_fields_anon'x5_1 "has_implicit_fields_anon'x5_1_x5_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x5_2@

    __defined at:__ @types\/anonymous\/struct.h 18:9@

    __exported by:__ @types\/anonymous\/struct.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x5_1_x5_2" Has_implicit_fields_anon'x5_1 ty where

  getField = RIP.getUnionPayload

{-| __C declaration:__ @x5_2@

    __defined at:__ @types\/anonymous\/struct.h 18:9@

    __exported by:__ @types\/anonymous\/struct.h@
-}
instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x5_1_x5_2" Has_implicit_fields_anon'x5_1 ty where

  hasField =
    \x0 ->
      (RIP.setUnionPayload, RIP.getField @"has_implicit_fields_anon'x5_1_x5_2" x0)

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_anon'x5_1_x5_2" (RIP.Ptr Has_implicit_fields_anon'x5_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x5_1_x5_2")

instance HasCField.HasCField Has_implicit_fields_anon'x5_1 "has_implicit_fields_anon'x5_1_x5_2" where

  type CFieldType Has_implicit_fields_anon'x5_1 "has_implicit_fields_anon'x5_1_x5_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct has_implicit_fields@

    __defined at:__ @types\/anonymous\/struct.h 5:8@

    __exported by:__ @types\/anonymous\/struct.h@
-}
data Has_implicit_fields = Has_implicit_fields
  { has_implicit_fields_x1 :: RIP.CInt
    {- ^ __C declaration:__ @x1@

         __defined at:__ @types\/anonymous\/struct.h 6:7@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_anon'x2_1 :: Has_implicit_fields_anon'x2_1
    {- ^ __C declaration:__ @anon\'x2_1@

         __defined at:__ @types\/anonymous\/struct.h 7:3@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_x3 :: RIP.CInt
    {- ^ __C declaration:__ @x3@

         __defined at:__ @types\/anonymous\/struct.h 11:7@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_anon'x4_1 :: Has_implicit_fields_anon'x4_1
    {- ^ __C declaration:__ @anon\'x4_1@

         __defined at:__ @types\/anonymous\/struct.h 12:3@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_anon'x5_1 :: Has_implicit_fields_anon'x5_1
    {- ^ __C declaration:__ @anon\'x5_1@

         __defined at:__ @types\/anonymous\/struct.h 16:3@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  , has_implicit_fields_x5 :: RIP.CInt
    {- ^ __C declaration:__ @x5@

         __defined at:__ @types\/anonymous\/struct.h 20:7@

         __exported by:__ @types\/anonymous\/struct.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize Has_implicit_fields where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Has_implicit_fields where

  readRaw =
    \ptr0 ->
          pure Has_implicit_fields
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x3") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_anon'x5_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x5") ptr0

instance Marshal.WriteRaw Has_implicit_fields where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Has_implicit_fields
            has_implicit_fields_x12
            has_implicit_fields_anon'x2_13
            has_implicit_fields_x34
            has_implicit_fields_anon'x4_15
            has_implicit_fields_anon'x5_16
            has_implicit_fields_x57 ->
                 HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x1") ptr0 has_implicit_fields_x12
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x2_1") ptr0 has_implicit_fields_anon'x2_13
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x3") ptr0 has_implicit_fields_x34
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x4_1") ptr0 has_implicit_fields_anon'x4_15
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_anon'x5_1") ptr0 has_implicit_fields_anon'x5_16
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x5") ptr0 has_implicit_fields_x57

deriving via Marshal.EquivStorable Has_implicit_fields instance RIP.Storable Has_implicit_fields

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_x1" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_x1 = y1
                              , has_implicit_fields_anon'x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1" x0
                              , has_implicit_fields_x3 = RIP.getField @"has_implicit_fields_x3" x0
                              , has_implicit_fields_anon'x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1" x0
                              , has_implicit_fields_anon'x5_1 = RIP.getField @"has_implicit_fields_anon'x5_1" x0
                              , has_implicit_fields_x5 = RIP.getField @"has_implicit_fields_x5" x0
                              }
      , RIP.getField @"has_implicit_fields_x1" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ Has_implicit_fields_anon'x2_1
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x2_1" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_anon'x2_1 = y1
                              , has_implicit_fields_x1 = RIP.getField @"has_implicit_fields_x1" x0
                              , has_implicit_fields_x3 = RIP.getField @"has_implicit_fields_x3" x0
                              , has_implicit_fields_anon'x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1" x0
                              , has_implicit_fields_anon'x5_1 = RIP.getField @"has_implicit_fields_anon'x5_1" x0
                              , has_implicit_fields_x5 = RIP.getField @"has_implicit_fields_x5" x0
                              }
      , RIP.getField @"has_implicit_fields_anon'x2_1" x0
      )

instance ( ty ~ Has_implicit_fields_anon'x2_1
         ) => RIP.HasField "has_implicit_fields_anon'x2_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x2_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_anon'x2_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_anon'x2_1" =
    Has_implicit_fields_anon'x2_1

  offset# = \_ -> \_ -> 4

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_x3" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_x3 = y1
                              , has_implicit_fields_x1 = RIP.getField @"has_implicit_fields_x1" x0
                              , has_implicit_fields_anon'x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1" x0
                              , has_implicit_fields_anon'x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1" x0
                              , has_implicit_fields_anon'x5_1 = RIP.getField @"has_implicit_fields_anon'x5_1" x0
                              , has_implicit_fields_x5 = RIP.getField @"has_implicit_fields_x5" x0
                              }
      , RIP.getField @"has_implicit_fields_x3" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x3" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x3")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x3" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x3" =
    RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( ty ~ Has_implicit_fields_anon'x4_1
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x4_1" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_anon'x4_1 = y1
                              , has_implicit_fields_x1 = RIP.getField @"has_implicit_fields_x1" x0
                              , has_implicit_fields_anon'x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1" x0
                              , has_implicit_fields_x3 = RIP.getField @"has_implicit_fields_x3" x0
                              , has_implicit_fields_anon'x5_1 = RIP.getField @"has_implicit_fields_anon'x5_1" x0
                              , has_implicit_fields_x5 = RIP.getField @"has_implicit_fields_x5" x0
                              }
      , RIP.getField @"has_implicit_fields_anon'x4_1" x0
      )

instance ( ty ~ Has_implicit_fields_anon'x4_1
         ) => RIP.HasField "has_implicit_fields_anon'x4_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x4_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_anon'x4_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_anon'x4_1" =
    Has_implicit_fields_anon'x4_1

  offset# = \_ -> \_ -> 16

instance ( ty ~ Has_implicit_fields_anon'x5_1
         ) => RIP.CompatHasField.HasField "has_implicit_fields_anon'x5_1" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_anon'x5_1 = y1
                              , has_implicit_fields_x1 = RIP.getField @"has_implicit_fields_x1" x0
                              , has_implicit_fields_anon'x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1" x0
                              , has_implicit_fields_x3 = RIP.getField @"has_implicit_fields_x3" x0
                              , has_implicit_fields_anon'x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1" x0
                              , has_implicit_fields_x5 = RIP.getField @"has_implicit_fields_x5" x0
                              }
      , RIP.getField @"has_implicit_fields_anon'x5_1" x0
      )

instance ( ty ~ Has_implicit_fields_anon'x5_1
         ) => RIP.HasField "has_implicit_fields_anon'x5_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_anon'x5_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_anon'x5_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_anon'x5_1" =
    Has_implicit_fields_anon'x5_1

  offset# = \_ -> \_ -> 24

instance ( ty ~ RIP.CInt
         ) => RIP.CompatHasField.HasField "has_implicit_fields_x5" Has_implicit_fields ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Has_implicit_fields { has_implicit_fields_x5 = y1
                              , has_implicit_fields_x1 = RIP.getField @"has_implicit_fields_x1" x0
                              , has_implicit_fields_anon'x2_1 = RIP.getField @"has_implicit_fields_anon'x2_1" x0
                              , has_implicit_fields_x3 = RIP.getField @"has_implicit_fields_x3" x0
                              , has_implicit_fields_anon'x4_1 = RIP.getField @"has_implicit_fields_anon'x4_1" x0
                              , has_implicit_fields_anon'x5_1 = RIP.getField @"has_implicit_fields_anon'x5_1" x0
                              }
      , RIP.getField @"has_implicit_fields_x5" x0
      )

instance ( ty ~ RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x5" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x5")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x5" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x5" =
    RIP.CInt

  offset# = \_ -> \_ -> 28
