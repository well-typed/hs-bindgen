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

module VendorDependencies.Uses_vendor_indirectly
    ( VendorDependencies.Uses_vendor_indirectly.Uses_vendor_indirectly(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified VendorDependencies.Uses_vendor
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct uses_vendor_indirectly@

    __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 7:8@

    __exported by:__ @vendor_dependencies.h@
-}
data Uses_vendor_indirectly = Uses_vendor_indirectly
  { uses_vendor_indirectly_last :: VendorDependencies.Uses_vendor.Uses_vendor
    {- ^ __C declaration:__ @last@

         __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 8:22@

         __exported by:__ @vendor_dependencies.h@
    -}
  , uses_vendor_indirectly_alpha :: BG.CDouble
    {- ^ __C declaration:__ @alpha@

         __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 9:10@

         __exported by:__ @vendor_dependencies.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Uses_vendor_indirectly where

  staticSizeOf = \_ -> (40 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Uses_vendor_indirectly where

  readRaw =
    \ptr0 ->
          pure Uses_vendor_indirectly
      <*> HasCField.readRaw (BG.Proxy @"uses_vendor_indirectly_last") ptr0
      <*> HasCField.readRaw (BG.Proxy @"uses_vendor_indirectly_alpha") ptr0

instance Marshal.WriteRaw Uses_vendor_indirectly where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Uses_vendor_indirectly
            uses_vendor_indirectly_last2
            uses_vendor_indirectly_alpha3 ->
                 HasCField.writeRaw (BG.Proxy @"uses_vendor_indirectly_last") ptr0 uses_vendor_indirectly_last2
              >> HasCField.writeRaw (BG.Proxy @"uses_vendor_indirectly_alpha") ptr0 uses_vendor_indirectly_alpha3

deriving via Marshal.EquivStorable Uses_vendor_indirectly instance BG.Storable Uses_vendor_indirectly

deriving via Struct.IsStructViaReadRaw Uses_vendor_indirectly instance Struct.IsStruct Uses_vendor_indirectly

{-| __C declaration:__ @last@

    __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 8:22@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ VendorDependencies.Uses_vendor.Uses_vendor
         ) => BG.CompatHasField.HasField "uses_vendor_indirectly_last" Uses_vendor_indirectly ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_vendor_indirectly { uses_vendor_indirectly_last = y1
                                 , uses_vendor_indirectly_alpha = BG.getField @"uses_vendor_indirectly_alpha" x0
                                 }
      , BG.getField @"uses_vendor_indirectly_last" x0
      )

instance ( ty ~ VendorDependencies.Uses_vendor.Uses_vendor
         ) => BG.HasField "uses_vendor_indirectly_last" (BG.Ptr Uses_vendor_indirectly) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_vendor_indirectly_last")

instance HasCField.HasCField Uses_vendor_indirectly "uses_vendor_indirectly_last" where

  type CFieldType Uses_vendor_indirectly "uses_vendor_indirectly_last" =
    VendorDependencies.Uses_vendor.Uses_vendor

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @alpha@

    __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 9:10@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "uses_vendor_indirectly_alpha" Uses_vendor_indirectly ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Uses_vendor_indirectly { uses_vendor_indirectly_alpha = y1
                                 , uses_vendor_indirectly_last = BG.getField @"uses_vendor_indirectly_last" x0
                                 }
      , BG.getField @"uses_vendor_indirectly_alpha" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "uses_vendor_indirectly_alpha" (BG.Ptr Uses_vendor_indirectly) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"uses_vendor_indirectly_alpha")

instance HasCField.HasCField Uses_vendor_indirectly "uses_vendor_indirectly_alpha" where

  type CFieldType Uses_vendor_indirectly "uses_vendor_indirectly_alpha" =
    BG.CDouble

  offset# = \_ -> \_ -> 32
