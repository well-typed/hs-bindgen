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
    ( Example.Has_implicit_fields_x2_1(..)
    , Example.Has_implicit_fields_x4_1(..)
    , Example.Has_implicit_fields_x5_1(..)
    , Example.get_has_implicit_fields_x5_1_x5_1
    , Example.set_has_implicit_fields_x5_1_x5_1
    , Example.get_has_implicit_fields_x5_1_x5_2
    , Example.set_has_implicit_fields_x5_1_x5_2
    , Example.Has_implicit_fields(..)
    , Example.get_has_implicit_fields_x1
    , Example.set_has_implicit_fields_x1
    , Example.get_has_implicit_fields_x2_1
    , Example.set_has_implicit_fields_x2_1
    , Example.get_has_implicit_fields_x3
    , Example.set_has_implicit_fields_x3
    , Example.get_has_implicit_fields_x4_1
    , Example.set_has_implicit_fields_x4_1
    , Example.get_has_implicit_fields_x5_1
    , Example.set_has_implicit_fields_x5_1
    , Example.get_has_implicit_fields_x5
    , Example.set_has_implicit_fields_x5
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct \@has_implicit_fields_x2_1@

    __defined at:__ @types\/anonymous\/union.h 5:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
data Has_implicit_fields_x2_1 = Has_implicit_fields_x2_1
  { has_implicit_fields_x2_1_x2_1 :: RIP.CInt
    {- ^ __C declaration:__ @x2_1@

         __defined at:__ @types\/anonymous\/union.h 6:9@

         __exported by:__ @types\/anonymous\/union.h@
    -}
  , has_implicit_fields_x2_1_x2_2 :: RIP.CInt
    {- ^ __C declaration:__ @x2_2@

         __defined at:__ @types\/anonymous\/union.h 7:9@

         __exported by:__ @types\/anonymous\/union.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Has_implicit_fields_x2_1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Has_implicit_fields_x2_1 where

  readRaw =
    \ptr0 ->
          pure Has_implicit_fields_x2_1
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x2_1_x2_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x2_1_x2_2") ptr0

instance Marshal.WriteRaw Has_implicit_fields_x2_1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Has_implicit_fields_x2_1
            has_implicit_fields_x2_1_x2_12
            has_implicit_fields_x2_1_x2_23 ->
                 HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x2_1_x2_1") ptr0 has_implicit_fields_x2_1_x2_12
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x2_1_x2_2") ptr0 has_implicit_fields_x2_1_x2_23

deriving via Marshal.EquivStorable Has_implicit_fields_x2_1 instance RIP.Storable Has_implicit_fields_x2_1

instance HasCField.HasCField Has_implicit_fields_x2_1 "has_implicit_fields_x2_1_x2_1" where

  type CFieldType Has_implicit_fields_x2_1 "has_implicit_fields_x2_1_x2_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x2_1_x2_1" (RIP.Ptr Has_implicit_fields_x2_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x2_1_x2_1")

instance HasCField.HasCField Has_implicit_fields_x2_1 "has_implicit_fields_x2_1_x2_2" where

  type CFieldType Has_implicit_fields_x2_1 "has_implicit_fields_x2_1_x2_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x2_1_x2_2" (RIP.Ptr Has_implicit_fields_x2_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x2_1_x2_2")

{-| __C declaration:__ @struct \@has_implicit_fields_x4_1@

    __defined at:__ @types\/anonymous\/union.h 10:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
data Has_implicit_fields_x4_1 = Has_implicit_fields_x4_1
  { has_implicit_fields_x4_1_x4_1 :: RIP.CInt
    {- ^ __C declaration:__ @x4_1@

         __defined at:__ @types\/anonymous\/union.h 11:9@

         __exported by:__ @types\/anonymous\/union.h@
    -}
  , has_implicit_fields_x4_1_x4_2 :: RIP.CInt
    {- ^ __C declaration:__ @x4_2@

         __defined at:__ @types\/anonymous\/union.h 12:9@

         __exported by:__ @types\/anonymous\/union.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Has_implicit_fields_x4_1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Has_implicit_fields_x4_1 where

  readRaw =
    \ptr0 ->
          pure Has_implicit_fields_x4_1
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x4_1_x4_1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"has_implicit_fields_x4_1_x4_2") ptr0

instance Marshal.WriteRaw Has_implicit_fields_x4_1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Has_implicit_fields_x4_1
            has_implicit_fields_x4_1_x4_12
            has_implicit_fields_x4_1_x4_23 ->
                 HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x4_1_x4_1") ptr0 has_implicit_fields_x4_1_x4_12
              >> HasCField.writeRaw (RIP.Proxy @"has_implicit_fields_x4_1_x4_2") ptr0 has_implicit_fields_x4_1_x4_23

deriving via Marshal.EquivStorable Has_implicit_fields_x4_1 instance RIP.Storable Has_implicit_fields_x4_1

instance HasCField.HasCField Has_implicit_fields_x4_1 "has_implicit_fields_x4_1_x4_1" where

  type CFieldType Has_implicit_fields_x4_1 "has_implicit_fields_x4_1_x4_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x4_1_x4_1" (RIP.Ptr Has_implicit_fields_x4_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x4_1_x4_1")

instance HasCField.HasCField Has_implicit_fields_x4_1 "has_implicit_fields_x4_1_x4_2" where

  type CFieldType Has_implicit_fields_x4_1 "has_implicit_fields_x4_1_x4_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x4_1_x4_2" (RIP.Ptr Has_implicit_fields_x4_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x4_1_x4_2")

{-| __C declaration:__ @union \@has_implicit_fields_x5_1@

    __defined at:__ @types\/anonymous\/union.h 14:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
newtype Has_implicit_fields_x5_1 = Has_implicit_fields_x5_1
  { unwrapHas_implicit_fields_x5_1 :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 4 4 instance Marshal.StaticSize Has_implicit_fields_x5_1

deriving via RIP.SizedByteArray 4 4 instance Marshal.ReadRaw Has_implicit_fields_x5_1

deriving via RIP.SizedByteArray 4 4 instance Marshal.WriteRaw Has_implicit_fields_x5_1

deriving via Marshal.EquivStorable Has_implicit_fields_x5_1 instance RIP.Storable Has_implicit_fields_x5_1

{-|

    __See:__ 'set_has_implicit_fields_x5_1_x5_1'

    __C declaration:__ @x5_1@

    __defined at:__ @types\/anonymous\/union.h 15:9@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x5_1_x5_1 ::
     Has_implicit_fields_x5_1
  -> RIP.CInt
get_has_implicit_fields_x5_1_x5_1 =
  RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x5_1_x5_1'

-}
set_has_implicit_fields_x5_1_x5_1 ::
     RIP.CInt
  -> Has_implicit_fields_x5_1
set_has_implicit_fields_x5_1_x5_1 =
  RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x5_1_x5_2'

    __C declaration:__ @x5_2@

    __defined at:__ @types\/anonymous\/union.h 16:9@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x5_1_x5_2 ::
     Has_implicit_fields_x5_1
  -> RIP.CInt
get_has_implicit_fields_x5_1_x5_2 =
  RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x5_1_x5_2'

-}
set_has_implicit_fields_x5_1_x5_2 ::
     RIP.CInt
  -> Has_implicit_fields_x5_1
set_has_implicit_fields_x5_1_x5_2 =
  RIP.setUnionPayload

instance HasCField.HasCField Has_implicit_fields_x5_1 "has_implicit_fields_x5_1_x5_1" where

  type CFieldType Has_implicit_fields_x5_1 "has_implicit_fields_x5_1_x5_1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x5_1_x5_1" (RIP.Ptr Has_implicit_fields_x5_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x5_1_x5_1")

instance HasCField.HasCField Has_implicit_fields_x5_1 "has_implicit_fields_x5_1_x5_2" where

  type CFieldType Has_implicit_fields_x5_1 "has_implicit_fields_x5_1_x5_2" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x5_1_x5_2" (RIP.Ptr Has_implicit_fields_x5_1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x5_1_x5_2")

{-| __C declaration:__ @union has_implicit_fields@

    __defined at:__ @types\/anonymous\/union.h 3:7@

    __exported by:__ @types\/anonymous\/union.h@
-}
newtype Has_implicit_fields = Has_implicit_fields
  { unwrapHas_implicit_fields :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via RIP.SizedByteArray 8 4 instance Marshal.StaticSize Has_implicit_fields

deriving via RIP.SizedByteArray 8 4 instance Marshal.ReadRaw Has_implicit_fields

deriving via RIP.SizedByteArray 8 4 instance Marshal.WriteRaw Has_implicit_fields

deriving via Marshal.EquivStorable Has_implicit_fields instance RIP.Storable Has_implicit_fields

{-|

    __See:__ 'set_has_implicit_fields_x1'

    __C declaration:__ @x1@

    __defined at:__ @types\/anonymous\/union.h 4:7@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x1 ::
     Has_implicit_fields
  -> RIP.CInt
get_has_implicit_fields_x1 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x1'

-}
set_has_implicit_fields_x1 ::
     RIP.CInt
  -> Has_implicit_fields
set_has_implicit_fields_x1 = RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x2_1'

    __C declaration:__ @x2_1@

    __defined at:__ @types\/anonymous\/union.h 5:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x2_1 ::
     Has_implicit_fields
  -> Has_implicit_fields_x2_1
get_has_implicit_fields_x2_1 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x2_1'

-}
set_has_implicit_fields_x2_1 ::
     Has_implicit_fields_x2_1
  -> Has_implicit_fields
set_has_implicit_fields_x2_1 = RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x3'

    __C declaration:__ @x3@

    __defined at:__ @types\/anonymous\/union.h 9:7@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x3 ::
     Has_implicit_fields
  -> RIP.CInt
get_has_implicit_fields_x3 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x3'

-}
set_has_implicit_fields_x3 ::
     RIP.CInt
  -> Has_implicit_fields
set_has_implicit_fields_x3 = RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x4_1'

    __C declaration:__ @x4_1@

    __defined at:__ @types\/anonymous\/union.h 10:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x4_1 ::
     Has_implicit_fields
  -> Has_implicit_fields_x4_1
get_has_implicit_fields_x4_1 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x4_1'

-}
set_has_implicit_fields_x4_1 ::
     Has_implicit_fields_x4_1
  -> Has_implicit_fields
set_has_implicit_fields_x4_1 = RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x5_1'

    __C declaration:__ @x5_1@

    __defined at:__ @types\/anonymous\/union.h 14:3@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x5_1 ::
     Has_implicit_fields
  -> Has_implicit_fields_x5_1
get_has_implicit_fields_x5_1 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x5_1'

-}
set_has_implicit_fields_x5_1 ::
     Has_implicit_fields_x5_1
  -> Has_implicit_fields
set_has_implicit_fields_x5_1 = RIP.setUnionPayload

{-|

    __See:__ 'set_has_implicit_fields_x5'

    __C declaration:__ @x5@

    __defined at:__ @types\/anonymous\/union.h 18:7@

    __exported by:__ @types\/anonymous\/union.h@
-}
get_has_implicit_fields_x5 ::
     Has_implicit_fields
  -> RIP.CInt
get_has_implicit_fields_x5 = RIP.getUnionPayload

{-|

    __See:__ 'get_has_implicit_fields_x5'

-}
set_has_implicit_fields_x5 ::
     RIP.CInt
  -> Has_implicit_fields
set_has_implicit_fields_x5 = RIP.setUnionPayload

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x1" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x2_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x2_1" =
    Has_implicit_fields_x2_1

  offset# = \_ -> \_ -> 0

instance ( (~) ty Has_implicit_fields_x2_1
         ) => RIP.HasField "has_implicit_fields_x2_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x2_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x3" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x3" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x3" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x3")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x4_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x4_1" =
    Has_implicit_fields_x4_1

  offset# = \_ -> \_ -> 0

instance ( (~) ty Has_implicit_fields_x4_1
         ) => RIP.HasField "has_implicit_fields_x4_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x4_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x5_1" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x5_1" =
    Has_implicit_fields_x5_1

  offset# = \_ -> \_ -> 0

instance ( (~) ty Has_implicit_fields_x5_1
         ) => RIP.HasField "has_implicit_fields_x5_1" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x5_1")

instance HasCField.HasCField Has_implicit_fields "has_implicit_fields_x5" where

  type CFieldType Has_implicit_fields "has_implicit_fields_x5" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( (~) ty RIP.CInt
         ) => RIP.HasField "has_implicit_fields_x5" (RIP.Ptr Has_implicit_fields) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"has_implicit_fields_x5")
