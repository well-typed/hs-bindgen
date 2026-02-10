{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct \@some_struct_field1@

    __defined at:__ @edge-cases\/anon_multiple_fields.h 5:3@

    __exported by:__ @edge-cases\/anon_multiple_fields.h@
-}
data Some_struct_field1 = Some_struct_field1
  { some_struct_field1_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:16@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  , some_struct_field1_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/anon_multiple_fields.h 5:23@

         __exported by:__ @edge-cases\/anon_multiple_fields.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Some_struct_field1 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Some_struct_field1 where

  readRaw =
    \ptr0 ->
          pure Some_struct_field1
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"some_struct_field1_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"some_struct_field1_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Some_struct_field1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct_field1 some_struct_field1_x2 some_struct_field1_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"some_struct_field1_x") ptr0 some_struct_field1_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"some_struct_field1_y") ptr0 some_struct_field1_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Some_struct_field1 instance F.Storable Some_struct_field1

instance HsBindgen.Runtime.HasCField.HasCField Some_struct_field1 "some_struct_field1_x" where

  type CFieldType Some_struct_field1 "some_struct_field1_x" =
    FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "some_struct_field1_x" (Ptr.Ptr Some_struct_field1) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"some_struct_field1_x")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct_field1 "some_struct_field1_y" where

  type CFieldType Some_struct_field1 "some_struct_field1_y" =
    FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "some_struct_field1_y" (Ptr.Ptr Some_struct_field1) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"some_struct_field1_y")

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
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Some_struct where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Some_struct where

  readRaw =
    \ptr0 ->
          pure Some_struct
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"some_struct_field1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"some_struct_field2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"some_struct_field3") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Some_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct some_struct_field12 some_struct_field23 some_struct_field34 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"some_struct_field1") ptr0 some_struct_field12
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"some_struct_field2") ptr0 some_struct_field23
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"some_struct_field3") ptr0 some_struct_field34

deriving via HsBindgen.Runtime.Marshal.EquivStorable Some_struct instance F.Storable Some_struct

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field1" where

  type CFieldType Some_struct "some_struct_field1" =
    Some_struct_field1

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "some_struct_field1" (Ptr.Ptr Some_struct) (Ptr.Ptr Some_struct_field1) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"some_struct_field1")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field2" where

  type CFieldType Some_struct "some_struct_field2" =
    Some_struct_field1

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "some_struct_field2" (Ptr.Ptr Some_struct) (Ptr.Ptr Some_struct_field1) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"some_struct_field2")

instance HsBindgen.Runtime.HasCField.HasCField Some_struct "some_struct_field3" where

  type CFieldType Some_struct "some_struct_field3" =
    Some_struct_field1

  offset# = \_ -> \_ -> 16

instance GHC.Records.HasField "some_struct_field3" (Ptr.Ptr Some_struct) (Ptr.Ptr Some_struct_field1) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"some_struct_field3")
