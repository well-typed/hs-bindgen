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
    ( M.Alias.Window_t(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct vendor_window@

    __defined at:__ @vendor\/window.h 4:8@

    __exported by:__ @root.h@
-}
data Window_t = Window_t
  { window_t_width :: BG.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @vendor\/window.h 4:28@

         __exported by:__ @root.h@
    -}
  , window_t_height :: BG.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @vendor\/window.h 4:39@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Window_t where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Window_t where

  readRaw =
    \ptr0 ->
          pure Window_t
      <*> HasCField.readRaw (BG.Proxy @"window_t_width") ptr0
      <*> HasCField.readRaw (BG.Proxy @"window_t_height") ptr0

instance Marshal.WriteRaw Window_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Window_t window_t_width2 window_t_height3 ->
               HasCField.writeRaw (BG.Proxy @"window_t_width") ptr0 window_t_width2
            >> HasCField.writeRaw (BG.Proxy @"window_t_height") ptr0 window_t_height3

deriving via Marshal.EquivStorable Window_t instance BG.Storable Window_t

deriving via Struct.IsStructViaReadRaw Window_t instance Struct.IsStruct Window_t

{-| __C declaration:__ @width@

    __defined at:__ @vendor\/window.h 4:28@

    __exported by:__ @root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "window_t_width" Window_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Window_t {window_t_width = y1, window_t_height = BG.getField @"window_t_height" x0}
      , BG.getField @"window_t_width" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "window_t_width" (BG.Ptr Window_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"window_t_width")

instance HasCField.HasCField Window_t "window_t_width" where

  type CFieldType Window_t "window_t_width" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @height@

    __defined at:__ @vendor\/window.h 4:39@

    __exported by:__ @root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "window_t_height" Window_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Window_t {window_t_height = y1, window_t_width = BG.getField @"window_t_width" x0}
      , BG.getField @"window_t_height" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "window_t_height" (BG.Ptr Window_t) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"window_t_height")

instance HasCField.HasCField Window_t "window_t_height" where

  type CFieldType Window_t "window_t_height" = BG.CInt

  offset# = \_ -> \_ -> 4
