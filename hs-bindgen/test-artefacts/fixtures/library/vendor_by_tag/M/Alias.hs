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

module M.Alias
    ( M.Alias.Vendor_window(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct vendor_window@

    __defined at:__ @vendor\/window.h 5:8@

    __exported by:__ @vendor_by_tag\/root.h@
-}
data Vendor_window = Vendor_window
  { vendor_window_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @vendor\/window.h 6:7@

         __exported by:__ @vendor_by_tag\/root.h@
    -}
  , vendor_window_height :: BG.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @vendor\/window.h 7:7@

         __exported by:__ @vendor_by_tag\/root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Vendor_window where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Vendor_window where

  readRaw =
    \ptr0 ->
          pure Vendor_window
      <*> HasCField.readRaw (BG.Proxy @"vendor_window_width") ptr0
      <*> HasCField.readRaw (BG.Proxy @"vendor_window_height") ptr0

instance Marshal.WriteRaw Vendor_window where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vendor_window vendor_window_width2 vendor_window_height3 ->
               HasCField.writeRaw (BG.Proxy @"vendor_window_width") ptr0 vendor_window_width2
            >> HasCField.writeRaw (BG.Proxy @"vendor_window_height") ptr0 vendor_window_height3

deriving via Marshal.EquivStorable Vendor_window instance BG.Storable Vendor_window

deriving via Struct.IsStructViaReadRaw Vendor_window instance Struct.IsStruct Vendor_window

{-| __C declaration:__ @width@

    __defined at:__ @vendor\/window.h 6:7@

    __exported by:__ @vendor_by_tag\/root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "vendor_window_width" Vendor_window ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vendor_window { vendor_window_width = y1
                        , vendor_window_height = BG.getField @"vendor_window_height" x0
                        }
      , BG.getField @"vendor_window_width" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "vendor_window_width" (BG.Ptr Vendor_window) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"vendor_window_width")

instance HasCField.HasCField Vendor_window "vendor_window_width" where

  type CFieldType Vendor_window "vendor_window_width" =
    BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @height@

    __defined at:__ @vendor\/window.h 7:7@

    __exported by:__ @vendor_by_tag\/root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "vendor_window_height" Vendor_window ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Vendor_window { vendor_window_height = y1
                        , vendor_window_width = BG.getField @"vendor_window_width" x0
                        }
      , BG.getField @"vendor_window_height" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "vendor_window_height" (BG.Ptr Vendor_window) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"vendor_window_height")

instance HasCField.HasCField Vendor_window "vendor_window_height" where

  type CFieldType Vendor_window "vendor_window_height" =
    BG.CInt

  offset# = \_ -> \_ -> 4
