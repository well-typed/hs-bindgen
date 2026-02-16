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

{-| __C declaration:__ @struct \@some_struct_field1@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:3@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct_field1 = Some_struct_field1
  { some_struct_field1_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:16@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field1_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:23@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Some_struct_field1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Some_struct_field1 where

  readRaw =
    \ptr0 ->
          pure Some_struct_field1
      <*> HasCField.readRaw (RIP.Proxy @"some_struct_field1_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"some_struct_field1_y") ptr0

instance Marshal.WriteRaw Some_struct_field1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct_field1 some_struct_field1_x2 some_struct_field1_y3 ->
               HasCField.writeRaw (RIP.Proxy @"some_struct_field1_x") ptr0 some_struct_field1_x2
            >> HasCField.writeRaw (RIP.Proxy @"some_struct_field1_y") ptr0 some_struct_field1_y3

deriving via Marshal.EquivStorable Some_struct_field1 instance RIP.Storable Some_struct_field1

instance HasCField.HasCField Some_struct_field1 "some_struct_field1_x" where

  type CFieldType Some_struct_field1 "some_struct_field1_x" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "some_struct_field1_x" (RIP.Ptr Some_struct_field1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"some_struct_field1_x")

instance HasCField.HasCField Some_struct_field1 "some_struct_field1_y" where

  type CFieldType Some_struct_field1 "some_struct_field1_y" =
    RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "some_struct_field1_y" (RIP.Ptr Some_struct_field1) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"some_struct_field1_y")

{-| __C declaration:__ @struct some_struct@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 4:8@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct = Some_struct
  { some_struct_field1 :: Some_struct_field1
    {- ^ __C declaration:__ @field1@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:28@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field2 :: Some_struct_field1
    {- ^ __C declaration:__ @field2@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:36@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field3 :: Some_struct_field1
    {- ^ __C declaration:__ @field3@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:44@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Some_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Some_struct where

  readRaw =
    \ptr0 ->
          pure Some_struct
      <*> HasCField.readRaw (RIP.Proxy @"some_struct_field1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"some_struct_field2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"some_struct_field3") ptr0

instance Marshal.WriteRaw Some_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct some_struct_field12 some_struct_field23 some_struct_field34 ->
               HasCField.writeRaw (RIP.Proxy @"some_struct_field1") ptr0 some_struct_field12
            >> HasCField.writeRaw (RIP.Proxy @"some_struct_field2") ptr0 some_struct_field23
            >> HasCField.writeRaw (RIP.Proxy @"some_struct_field3") ptr0 some_struct_field34

deriving via Marshal.EquivStorable Some_struct instance RIP.Storable Some_struct

instance HasCField.HasCField Some_struct "some_struct_field1" where

  type CFieldType Some_struct "some_struct_field1" =
    Some_struct_field1

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Some_struct_field1
         ) => RIP.HasField "some_struct_field1" (RIP.Ptr Some_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"some_struct_field1")

instance HasCField.HasCField Some_struct "some_struct_field2" where

  type CFieldType Some_struct "some_struct_field2" =
    Some_struct_field1

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) Some_struct_field1
         ) => RIP.HasField "some_struct_field2" (RIP.Ptr Some_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"some_struct_field2")

instance HasCField.HasCField Some_struct "some_struct_field3" where

  type CFieldType Some_struct "some_struct_field3" =
    Some_struct_field1

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) Some_struct_field1
         ) => RIP.HasField "some_struct_field3" (RIP.Ptr Some_struct) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"some_struct_field3")
