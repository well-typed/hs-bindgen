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

module VendorDependencies.Selected
    ( VendorDependencies.Selected.Selected(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified VendorDependencies.No_vendor
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct selected@

    __defined at:__ @vendor_dependencies\/selected.h 8:8@

    __exported by:__ @vendor_dependencies.h@
-}
data Selected = Selected
  { selected_dependency :: PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
    {- ^ __C declaration:__ @dependency@

         __defined at:__ @vendor_dependencies\/selected.h 9:27@

         __exported by:__ @vendor_dependencies.h@
    -}
  , selected_offset :: BG.CDouble
    {- ^ __C declaration:__ @offset@

         __defined at:__ @vendor_dependencies\/selected.h 10:10@

         __exported by:__ @vendor_dependencies.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Selected where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Selected where

  readRaw =
    \ptr0 ->
          pure Selected
      <*> HasCField.readRaw (BG.Proxy @"selected_dependency") ptr0
      <*> HasCField.readRaw (BG.Proxy @"selected_offset") ptr0

instance Marshal.WriteRaw Selected where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Selected selected_dependency2 selected_offset3 ->
               HasCField.writeRaw (BG.Proxy @"selected_dependency") ptr0 selected_dependency2
            >> HasCField.writeRaw (BG.Proxy @"selected_offset") ptr0 selected_offset3

deriving via Marshal.EquivStorable Selected instance BG.Storable Selected

deriving via Struct.IsStructViaReadRaw Selected instance Struct.IsStruct Selected

{-| __C declaration:__ @dependency@

    __defined at:__ @vendor_dependencies\/selected.h 9:27@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
         ) => BG.CompatHasField.HasField "selected_dependency" Selected ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Selected {selected_dependency = y1, selected_offset = BG.getField @"selected_offset" x0}
      , BG.getField @"selected_dependency" x0
      )

instance ( ty ~ PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
         ) => BG.HasField "selected_dependency" (BG.Ptr Selected) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"selected_dependency")

instance HasCField.HasCField Selected "selected_dependency" where

  type CFieldType Selected "selected_dependency" =
    PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @offset@

    __defined at:__ @vendor_dependencies\/selected.h 10:10@

    __exported by:__ @vendor_dependencies.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "selected_offset" Selected ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Selected { selected_offset = y1
                   , selected_dependency = BG.getField @"selected_dependency" x0
                   }
      , BG.getField @"selected_offset" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "selected_offset" (BG.Ptr Selected) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"selected_offset")

instance HasCField.HasCField Selected "selected_offset" where

  type CFieldType Selected "selected_offset" =
    BG.CDouble

  offset# = \_ -> \_ -> 8
