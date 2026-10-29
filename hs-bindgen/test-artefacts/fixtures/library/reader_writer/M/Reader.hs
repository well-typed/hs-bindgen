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

module M.Reader
    ( M.Reader.Handle(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct handle@

    __defined at:__ @detail.h 4:8@

    __exported by:__ @root.h@
-}
data Handle = Handle
  { handle_fd :: BG.CInt
    {- ^ __C declaration:__ @fd@

         __defined at:__ @detail.h 4:21@

         __exported by:__ @root.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Handle where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Handle where

  readRaw =
    \ptr0 ->
          pure Handle
      <*> HasCField.readRaw (BG.Proxy @"handle_fd") ptr0

instance Marshal.WriteRaw Handle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Handle handle_fd2 ->
            HasCField.writeRaw (BG.Proxy @"handle_fd") ptr0 handle_fd2

deriving via Marshal.EquivStorable Handle instance BG.Storable Handle

deriving via Struct.IsStructViaReadRaw Handle instance Struct.IsStruct Handle

{-| __C declaration:__ @fd@

    __defined at:__ @detail.h 4:21@

    __exported by:__ @root.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "handle_fd" Handle ty where

  hasField =
    \x0 ->
      (\y1 ->
         Handle {handle_fd = y1}, BG.getField @"handle_fd" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "handle_fd" (BG.Ptr Handle) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"handle_fd")

instance HasCField.HasCField Handle "handle_fd" where

  type CFieldType Handle "handle_fd" = BG.CInt

  offset# = \_ -> \_ -> 0
