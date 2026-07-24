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
    ( Example.UnionA(..)
    , Example.ExA(..)
    , Example.ExB_fieldB1(..)
    , Example.ExB(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union unionA@

    __defined at:__ @types\/unions\/nested_unions.h 2:15@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype UnionA = UnionA
  { unwrapUnionA :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize UnionA

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw UnionA

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw UnionA

deriving via Marshal.EquivStorable UnionA instance BG.Storable UnionA

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion UnionA

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/nested_unions.h 3:21@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "unionA_a" UnionA ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/nested_unions.h 3:21@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "unionA_a" UnionA ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"unionA_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unionA_a" (BG.Ptr UnionA) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unionA_a")

instance HasCField.HasCField UnionA "unionA_a" where

  type CFieldType UnionA "unionA_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/nested_unions.h 4:22@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ BG.CChar) => BG.HasField "unionA_b" UnionA ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/nested_unions.h 4:22@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "unionA_b" UnionA ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"unionA_b" x0)

instance ( ty ~ BG.CChar
         ) => BG.HasField "unionA_b" (BG.Ptr UnionA) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unionA_b")

instance HasCField.HasCField UnionA "unionA_b" where

  type CFieldType UnionA "unionA_b" = BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct exA@

    __defined at:__ @types\/unions\/nested_unions.h 1:8@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
data ExA = ExA
  { exA_fieldA1 :: UnionA
    {- ^ __C declaration:__ @fieldA1@

         __defined at:__ @types\/unions\/nested_unions.h 5:11@

         __exported by:__ @types\/unions\/nested_unions.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize ExA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExA where

  readRaw =
    \ptr0 ->
          pure ExA
      <*> HasCField.readRaw (BG.Proxy @"exA_fieldA1") ptr0

instance Marshal.WriteRaw ExA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HasCField.writeRaw (BG.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

deriving via Marshal.EquivStorable ExA instance BG.Storable ExA

deriving via Struct.IsStructViaStorable ExA instance Struct.IsStruct ExA

{-| __C declaration:__ @fieldA1@

    __defined at:__ @types\/unions\/nested_unions.h 5:11@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ UnionA) => BG.CompatHasField.HasField "exA_fieldA1" ExA ty where

  hasField =
    \x0 ->
      (\y1 ->
         ExA {exA_fieldA1 = y1}, BG.getField @"exA_fieldA1" x0)

instance ( ty ~ UnionA
         ) => BG.HasField "exA_fieldA1" (BG.Ptr ExA) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exA_fieldA1")

instance HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = UnionA

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@exB_fieldB1@

    __defined at:__ @types\/unions\/nested_unions.h 9:9@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { unwrapExB_fieldB1 :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize ExB_fieldB1

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw ExB_fieldB1

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw ExB_fieldB1

deriving via Marshal.EquivStorable ExB_fieldB1 instance BG.Storable ExB_fieldB1

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion ExB_fieldB1

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/nested_unions.h 10:21@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "exB_fieldB1_a" ExB_fieldB1 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @a@

    __defined at:__ @types\/unions\/nested_unions.h 10:21@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "exB_fieldB1_a" ExB_fieldB1 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"exB_fieldB1_a" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "exB_fieldB1_a" (BG.Ptr ExB_fieldB1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exB_fieldB1_a")

instance HasCField.HasCField ExB_fieldB1 "exB_fieldB1_a" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/nested_unions.h 11:22@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance (ty ~ BG.CChar) => BG.HasField "exB_fieldB1_b" ExB_fieldB1 ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @b@

    __defined at:__ @types\/unions\/nested_unions.h 11:22@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "exB_fieldB1_b" ExB_fieldB1 ty where

  hasField =
    \x0 ->
      (BG.setUnionPayload, BG.getField @"exB_fieldB1_b" x0)

instance ( ty ~ BG.CChar
         ) => BG.HasField "exB_fieldB1_b" (BG.Ptr ExB_fieldB1) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exB_fieldB1_b")

instance HasCField.HasCField ExB_fieldB1 "exB_fieldB1_b" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_b" =
    BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct exB@

    __defined at:__ @types\/unions\/nested_unions.h 8:8@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
    {- ^ __C declaration:__ @fieldB1@

         __defined at:__ @types\/unions\/nested_unions.h 12:11@

         __exported by:__ @types\/unions\/nested_unions.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize ExB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExB where

  readRaw =
    \ptr0 ->
          pure ExB
      <*> HasCField.readRaw (BG.Proxy @"exB_fieldB1") ptr0

instance Marshal.WriteRaw ExB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HasCField.writeRaw (BG.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

deriving via Marshal.EquivStorable ExB instance BG.Storable ExB

deriving via Struct.IsStructViaStorable ExB instance Struct.IsStruct ExB

{-| __C declaration:__ @fieldB1@

    __defined at:__ @types\/unions\/nested_unions.h 12:11@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
instance ( ty ~ ExB_fieldB1
         ) => BG.CompatHasField.HasField "exB_fieldB1" ExB ty where

  hasField =
    \x0 ->
      (\y1 ->
         ExB {exB_fieldB1 = y1}, BG.getField @"exB_fieldB1" x0)

instance ( ty ~ ExB_fieldB1
         ) => BG.HasField "exB_fieldB1" (BG.Ptr ExB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"exB_fieldB1")

instance HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0
