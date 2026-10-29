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
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct window@

    __defined at:__ @window.h 5:8@

    __exported by:__ @root.h@
-}
data Window = Window
  { window_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @window.h 5:21@

         __exported by:__ @root.h@
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

    __defined at:__ @window.h 5:21@

    __exported by:__ @root.h@
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
