{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Copies of declarations from outside the @--library@ directories

    The selection keeps them, usually because program slicing pulls in what the library uses.

    To share them between libraries, generate their bindings once, and pass that binding specification with @--external-binding-spec@ to each library run.
-}
module VendorDependencies.Vendor
    ( VendorDependencies.Vendor.Vendor_typedef(..)
    , VendorDependencies.Vendor.Vendor_struct(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @vendor_typedef@

    __defined at:__ @vendor\/vendor.h 6:14@

    __exported by:__ @vendor_dependencies.h@
-}
newtype Vendor_typedef = Vendor_typedef
  { unwrapVendor_typedef :: BG.CLong
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "unwrapVendor_typedef" Vendor_typedef ty where

  hasField =
    \x0 ->
      ( \y1 -> Vendor_typedef {unwrapVendor_typedef = y1}
      , BG.getField @"unwrapVendor_typedef" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "unwrapVendor_typedef" (BG.Ptr Vendor_typedef) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapVendor_typedef")

instance HasCField.HasCField Vendor_typedef "unwrapVendor_typedef" where

  type CFieldType Vendor_typedef "unwrapVendor_typedef" =
    BG.CLong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct vendor_struct@

    __defined at:__ @vendor\/vendor.h 8:8@

    __exported by:__ @vendor_dependencies.h@
-}
data Vendor_struct = Vendor_struct
  { vendor_struct_a :: Vendor_typedef
    {- ^ __C declaration:__ @a@

         __defined at:__ @vendor\/vendor.h 9:18@

         __exported by:__ @vendor_dependencies.h@
    -}
  , vendor_struct_b :: Vendor_typedef
    {- ^ __C declaration:__ @b@

         __defined at:__ @vendor\/vendor.h 10:18@

         __exported by:__ @vendor_dependencies.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Vendor_struct where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Vendor_struct where

  readRaw =
    \ptr0 ->
          pure Vendor_struct
      <*> HasCField.readRaw (BG.Proxy @"vendor_struct_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"vendor_struct_b") ptr0

instance Marshal.WriteRaw Vendor_struct where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vendor_struct vendor_struct_a2 vendor_struct_b3 ->
               HasCField.writeRaw (BG.Proxy @"vendor_struct_a") ptr0 vendor_struct_a2
            >> HasCField.writeRaw (BG.Proxy @"vendor_struct_b") ptr0 vendor_struct_b3

deriving via Marshal.EquivStorable Vendor_struct instance BG.Storable Vendor_struct

deriving via Struct.IsStructViaReadRaw Vendor_struct instance Struct.IsStruct Vendor_struct

{-| __C declaration:__ @a@

    __defined at:__ @vendor\/vendor.h 9:18@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ Vendor_typedef
         ) => BG.CompatHasField.HasField "vendor_struct_a" Vendor_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vendor_struct {vendor_struct_a = y1, vendor_struct_b = BG.getField @"vendor_struct_b" x0}
      , BG.getField @"vendor_struct_a" x0
      )

instance ( ty ~ Vendor_typedef
         ) => BG.HasField "vendor_struct_a" (BG.Ptr Vendor_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"vendor_struct_a")

instance HasCField.HasCField Vendor_struct "vendor_struct_a" where

  type CFieldType Vendor_struct "vendor_struct_a" =
    Vendor_typedef

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @vendor\/vendor.h 10:18@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ Vendor_typedef
         ) => BG.CompatHasField.HasField "vendor_struct_b" Vendor_struct ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vendor_struct {vendor_struct_b = y1, vendor_struct_a = BG.getField @"vendor_struct_a" x0}
      , BG.getField @"vendor_struct_b" x0
      )

instance ( ty ~ Vendor_typedef
         ) => BG.HasField "vendor_struct_b" (BG.Ptr Vendor_struct) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"vendor_struct_b")

instance HasCField.HasCField Vendor_struct "vendor_struct_b" where

  type CFieldType Vendor_struct "vendor_struct_b" =
    Vendor_typedef

  offset# = \_ -> \_ -> 8
