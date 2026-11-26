{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module VendorDependencies.Uses_vendor
    ( VendorDependencies.Uses_vendor.Uses_vendor(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified VendorDependencies.No_vendor
import qualified VendorDependencies.Vendor
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct uses_vendor@

    __defined at:__ @uses_vendor.h 10:8@

    __exported by:__ @vendor_dependencies.h@
-}
data Uses_vendor = Uses_vendor
  { uses_vendor_source :: PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
    {- ^ __C declaration:__ @source@

         __defined at:__ @uses_vendor.h 11:27@

         __exported by:__ @vendor_dependencies.h@
    -}
  , uses_vendor_taken_at :: VendorDependencies.Vendor.Vendor_struct
    {- ^ __C declaration:__ @taken_at@

         __defined at:__ @uses_vendor.h 12:24@

         __exported by:__ @vendor_dependencies.h@
    -}
  , uses_vendor_value :: BG.CDouble
    {- ^ __C declaration:__ @value@

         __defined at:__ @uses_vendor.h 13:10@

         __exported by:__ @vendor_dependencies.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Uses_vendor where

  staticSizeOf = \_ -> (32 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Uses_vendor where

  readRaw =
    \ptr0 ->
          pure Uses_vendor
      <*> HasCField.readRaw (BG.Proxy @"uses_vendor_source") ptr0
      <*> HasCField.readRaw (BG.Proxy @"uses_vendor_taken_at") ptr0
      <*> HasCField.readRaw (BG.Proxy @"uses_vendor_value") ptr0

instance Marshal.WriteRaw Uses_vendor where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Uses_vendor uses_vendor_source2 uses_vendor_taken_at3 uses_vendor_value4 ->
               HasCField.writeRaw (BG.Proxy @"uses_vendor_source") ptr0 uses_vendor_source2
            >> HasCField.writeRaw (BG.Proxy @"uses_vendor_taken_at") ptr0 uses_vendor_taken_at3
            >> HasCField.writeRaw (BG.Proxy @"uses_vendor_value") ptr0 uses_vendor_value4

deriving via Marshal.EquivStorable Uses_vendor instance BG.Storable Uses_vendor

deriving via Struct.IsStructViaReadRaw Uses_vendor instance Struct.IsStruct Uses_vendor

{-| __C declaration:__ @source@

    __defined at:__ @uses_vendor.h 11:27@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
         ) => BG.CompatHasField.HasField "uses_vendor_source" Uses_vendor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_vendor { uses_vendor_source = y1
                      , uses_vendor_taken_at = BG.getField @"uses_vendor_taken_at" x0
                      , uses_vendor_value = BG.getField @"uses_vendor_value" x0
                      }
      , BG.getField @"uses_vendor_source" x0
      )

instance ( ty ~ PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
         ) => BG.HasField "uses_vendor_source" (BG.Ptr Uses_vendor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_vendor_source")

instance HasCField.HasCField Uses_vendor "uses_vendor_source" where

  type CFieldType Uses_vendor "uses_vendor_source" =
    PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @taken_at@

    __defined at:__ @uses_vendor.h 12:24@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ VendorDependencies.Vendor.Vendor_struct
         ) => BG.CompatHasField.HasField "uses_vendor_taken_at" Uses_vendor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_vendor { uses_vendor_taken_at = y1
                      , uses_vendor_source = BG.getField @"uses_vendor_source" x0
                      , uses_vendor_value = BG.getField @"uses_vendor_value" x0
                      }
      , BG.getField @"uses_vendor_taken_at" x0
      )

instance ( ty ~ VendorDependencies.Vendor.Vendor_struct
         ) => BG.HasField "uses_vendor_taken_at" (BG.Ptr Uses_vendor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_vendor_taken_at")

instance HasCField.HasCField Uses_vendor "uses_vendor_taken_at" where

  type CFieldType Uses_vendor "uses_vendor_taken_at" =
    VendorDependencies.Vendor.Vendor_struct

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @value@

    __defined at:__ @uses_vendor.h 13:10@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "uses_vendor_value" Uses_vendor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_vendor { uses_vendor_value = y1
                      , uses_vendor_source = BG.getField @"uses_vendor_source" x0
                      , uses_vendor_taken_at = BG.getField @"uses_vendor_taken_at" x0
                      }
      , BG.getField @"uses_vendor_value" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "uses_vendor_value" (BG.Ptr Uses_vendor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_vendor_value")

instance HasCField.HasCField Uses_vendor "uses_vendor_value" where

  type CFieldType Uses_vendor "uses_vendor_value" =
    BG.CDouble

  offset# = \_ -> \_ -> 24
