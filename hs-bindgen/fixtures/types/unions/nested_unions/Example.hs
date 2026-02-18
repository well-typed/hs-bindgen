{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @union unionA@

    __defined at:__ @types\/unions\/nested_unions.h 2:15@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype UnionA = UnionA
  { unwrapUnionA :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize UnionA

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw UnionA

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw UnionA

deriving via Marshal.EquivStorable UnionA instance RIP.Storable UnionA

{-|

  __See:__ 'set_unionA_a'

__C declaration:__ @a@

__defined at:__ @types\/unions\/nested_unions.h 3:21@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_unionA_a ::
     UnionA
  -> RIP.CInt
get_unionA_a = RIP.getUnionPayload

{-|

  __See:__ 'get_unionA_a'

-}
set_unionA_a ::
     RIP.CInt
  -> UnionA
set_unionA_a = RIP.setUnionPayload

{-|

  __See:__ 'set_unionA_b'

__C declaration:__ @b@

__defined at:__ @types\/unions\/nested_unions.h 4:22@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_unionA_b ::
     UnionA
  -> RIP.CChar
get_unionA_b = RIP.getUnionPayload

{-|

  __See:__ 'get_unionA_b'

-}
set_unionA_b ::
     RIP.CChar
  -> UnionA
set_unionA_b = RIP.setUnionPayload

instance HasCField.HasCField UnionA "unionA_a" where

  type CFieldType UnionA "unionA_a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unionA_a" (RIP.Ptr UnionA) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unionA_a")

instance HasCField.HasCField UnionA "unionA_b" where

  type CFieldType UnionA "unionA_b" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "unionA_b" (RIP.Ptr UnionA) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unionA_b")

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
  deriving stock (RIP.Generic)

instance Marshal.StaticSize ExA where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExA where

  readRaw =
    \ptr0 ->
          pure ExA
      <*> HasCField.readRaw (RIP.Proxy @"exA_fieldA1") ptr0

instance Marshal.WriteRaw ExA where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 ->
            HasCField.writeRaw (RIP.Proxy @"exA_fieldA1") ptr0 exA_fieldA12

deriving via Marshal.EquivStorable ExA instance RIP.Storable ExA

instance HasCField.HasCField ExA "exA_fieldA1" where

  type CFieldType ExA "exA_fieldA1" = UnionA

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) UnionA
         ) => RIP.HasField "exA_fieldA1" (RIP.Ptr ExA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exA_fieldA1")

{-| __C declaration:__ @union \@exB_fieldB1@

    __defined at:__ @types\/unions\/nested_unions.h 9:9@

    __exported by:__ @types\/unions\/nested_unions.h@
-}
newtype ExB_fieldB1 = ExB_fieldB1
  { unwrapExB_fieldB1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize ExB_fieldB1

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw ExB_fieldB1

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw ExB_fieldB1

deriving via Marshal.EquivStorable ExB_fieldB1 instance RIP.Storable ExB_fieldB1

{-|

  __See:__ 'set_exB_fieldB1_a'

__C declaration:__ @a@

__defined at:__ @types\/unions\/nested_unions.h 10:21@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_exB_fieldB1_a ::
     ExB_fieldB1
  -> RIP.CInt
get_exB_fieldB1_a = RIP.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_a'

-}
set_exB_fieldB1_a ::
     RIP.CInt
  -> ExB_fieldB1
set_exB_fieldB1_a = RIP.setUnionPayload

{-|

  __See:__ 'set_exB_fieldB1_b'

__C declaration:__ @b@

__defined at:__ @types\/unions\/nested_unions.h 11:22@

__exported by:__ @types\/unions\/nested_unions.h@
-}
get_exB_fieldB1_b ::
     ExB_fieldB1
  -> RIP.CChar
get_exB_fieldB1_b = RIP.getUnionPayload

{-|

  __See:__ 'get_exB_fieldB1_b'

-}
set_exB_fieldB1_b ::
     RIP.CChar
  -> ExB_fieldB1
set_exB_fieldB1_b = RIP.setUnionPayload

instance HasCField.HasCField ExB_fieldB1 "exB_fieldB1_a" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_a" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "exB_fieldB1_a" (RIP.Ptr ExB_fieldB1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exB_fieldB1_a")

instance HasCField.HasCField ExB_fieldB1 "exB_fieldB1_b" where

  type CFieldType ExB_fieldB1 "exB_fieldB1_b" =
    RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "exB_fieldB1_b" (RIP.Ptr ExB_fieldB1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exB_fieldB1_b")

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
  deriving stock (RIP.Generic)

instance Marshal.StaticSize ExB where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw ExB where

  readRaw =
    \ptr0 ->
          pure ExB
      <*> HasCField.readRaw (RIP.Proxy @"exB_fieldB1") ptr0

instance Marshal.WriteRaw ExB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 ->
            HasCField.writeRaw (RIP.Proxy @"exB_fieldB1") ptr0 exB_fieldB12

deriving via Marshal.EquivStorable ExB instance RIP.Storable ExB

instance HasCField.HasCField ExB "exB_fieldB1" where

  type CFieldType ExB "exB_fieldB1" = ExB_fieldB1

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ExB_fieldB1
         ) => RIP.HasField "exB_fieldB1" (RIP.Ptr ExB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"exB_fieldB1")
