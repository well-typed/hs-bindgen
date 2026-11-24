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

module VendorDependencies.No_vendor
    ( VendorDependencies.No_vendor.No_vendor(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct no_vendor@

    __defined at:__ @no_vendor.h 4:8@

    __exported by:__ @vendor_dependencies.h@
-}
data No_vendor = No_vendor
  { no_vendor_name :: PtrConst.PtrConst BG.CChar
    {- ^ __C declaration:__ @name@

         __defined at:__ @no_vendor.h 5:15@

         __exported by:__ @vendor_dependencies.h@
    -}
  , no_vendor_id :: BG.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @no_vendor.h 6:7@

         __exported by:__ @vendor_dependencies.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize No_vendor where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw No_vendor where

  readRaw =
    \ptr0 ->
          pure No_vendor
      <*> HasCField.readRaw (BG.Proxy @"no_vendor_name") ptr0
      <*> HasCField.readRaw (BG.Proxy @"no_vendor_id") ptr0

instance Marshal.WriteRaw No_vendor where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          No_vendor no_vendor_name2 no_vendor_id3 ->
               HasCField.writeRaw (BG.Proxy @"no_vendor_name") ptr0 no_vendor_name2
            >> HasCField.writeRaw (BG.Proxy @"no_vendor_id") ptr0 no_vendor_id3

deriving via Marshal.EquivStorable No_vendor instance BG.Storable No_vendor

deriving via Struct.IsStructViaReadRaw No_vendor instance Struct.IsStruct No_vendor

{-| __C declaration:__ @name@

    __defined at:__ @no_vendor.h 5:15@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ PtrConst.PtrConst BG.CChar
         ) => BG.CompatHasField.HasField "no_vendor_name" No_vendor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          No_vendor {no_vendor_name = y1, no_vendor_id = BG.getField @"no_vendor_id" x0}
      , BG.getField @"no_vendor_name" x0
      )

instance ( ty ~ PtrConst.PtrConst BG.CChar
         ) => BG.HasField "no_vendor_name" (BG.Ptr No_vendor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"no_vendor_name")

instance HasCField.HasCField No_vendor "no_vendor_name" where

  type CFieldType No_vendor "no_vendor_name" =
    PtrConst.PtrConst BG.CChar

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @id@

    __defined at:__ @no_vendor.h 6:7@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "no_vendor_id" No_vendor ty where

  hasField =
    \x0 ->
      ( \y1 ->
          No_vendor {no_vendor_id = y1, no_vendor_name = BG.getField @"no_vendor_name" x0}
      , BG.getField @"no_vendor_id" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "no_vendor_id" (BG.Ptr No_vendor) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"no_vendor_id")

instance HasCField.HasCField No_vendor "no_vendor_id" where

  type CFieldType No_vendor "no_vendor_id" = BG.CInt

  offset# = \_ -> \_ -> 8
