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

module M.Window
    ( M.Window.Window(..)
    , M.Window.Widget(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct window@

    __defined at:__ @window.h 8:8@

    __exported by:__ @window_widget\/root.h@
-}
data Window = Window
  { window_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @window.h 9:7@

         __exported by:__ @window_widget\/root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Window where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Window where

  readRaw =
    \ptr0 ->
          pure Window
      <*> HasCField.readRaw (BG.Proxy @"window_width") ptr0

instance Marshal.WriteRaw Window where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Window window_width2 ->
            HasCField.writeRaw (BG.Proxy @"window_width") ptr0 window_width2

deriving via Marshal.EquivStorable Window instance BG.Storable Window

deriving via Struct.IsStructViaReadRaw Window instance Struct.IsStruct Window

{-| __C declaration:__ @width@

    __defined at:__ @window.h 9:7@

    __exported by:__ @window_widget\/root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "window_width" Window ty where

  hasField =
    \x0 ->
      (\y1 ->
         Window {window_width = y1}, BG.getField @"window_width" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "window_width" (BG.Ptr Window) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"window_width")

instance HasCField.HasCField Window "window_width" where

  type CFieldType Window "window_width" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct widget@

    __defined at:__ @widget.h 8:8@

    __exported by:__ @window_widget\/root.h@
-}
data Widget = Widget
  { widget_id :: BG.CInt
    {- ^ __C declaration:__ @id@

         __defined at:__ @widget.h 9:7@

         __exported by:__ @window_widget\/root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Widget where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Widget where

  readRaw =
    \ptr0 ->
          pure Widget
      <*> HasCField.readRaw (BG.Proxy @"widget_id") ptr0

instance Marshal.WriteRaw Widget where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Widget widget_id2 ->
            HasCField.writeRaw (BG.Proxy @"widget_id") ptr0 widget_id2

deriving via Marshal.EquivStorable Widget instance BG.Storable Widget

deriving via Struct.IsStructViaReadRaw Widget instance Struct.IsStruct Widget

{-| __C declaration:__ @id@

    __defined at:__ @widget.h 9:7@

    __exported by:__ @window_widget\/root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "widget_id" Widget ty where

  hasField =
    \x0 ->
      (\y1 ->
         Widget {widget_id = y1}, BG.getField @"widget_id" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "widget_id" (BG.Ptr Widget) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"widget_id")

instance HasCField.HasCField Widget "widget_id" where

  type CFieldType Widget "widget_id" = BG.CInt

  offset# = \_ -> \_ -> 0
