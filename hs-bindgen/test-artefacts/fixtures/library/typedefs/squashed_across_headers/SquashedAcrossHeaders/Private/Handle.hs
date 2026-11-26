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

module SquashedAcrossHeaders.Private.Handle
    ( SquashedAcrossHeaders.Private.Handle.Handle(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified SquashedAcrossHeaders.A
import Prelude ((<*>), (>>), Eq, Int, Show, pure, type (~))

{-| __C declaration:__ @struct handle@

    __defined at:__ @squashed_across_headers\/private\/handle.h 6:8@

    __exported by:__ @squashed_across_headers.h@
-}
data Handle = Handle
  { handle_owner :: BG.Ptr SquashedAcrossHeaders.A.A
    {- ^ __C declaration:__ @owner@

         __defined at:__ @squashed_across_headers\/private\/handle.h 7:6@

         __exported by:__ @squashed_across_headers.h@
    -}
  , handle_uses :: BG.CInt
    {- ^ __C declaration:__ @uses@

         __defined at:__ @squashed_across_headers\/private\/handle.h 8:7@

         __exported by:__ @squashed_across_headers.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Handle where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Handle where

  readRaw =
    \ptr0 ->
          pure Handle
      <*> HasCField.readRaw (BG.Proxy @"handle_owner") ptr0
      <*> HasCField.readRaw (BG.Proxy @"handle_uses") ptr0

instance Marshal.WriteRaw Handle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Handle handle_owner2 handle_uses3 ->
               HasCField.writeRaw (BG.Proxy @"handle_owner") ptr0 handle_owner2
            >> HasCField.writeRaw (BG.Proxy @"handle_uses") ptr0 handle_uses3

deriving via Marshal.EquivStorable Handle instance BG.Storable Handle

deriving via Struct.IsStructViaReadRaw Handle instance Struct.IsStruct Handle

{-| __C declaration:__ @owner@

    __defined at:__ @squashed_across_headers\/private\/handle.h 7:6@

    __exported by:__ @squashed_across_headers.h@
-}
instance ( ty ~ BG.Ptr SquashedAcrossHeaders.A.A
         ) => BG.CompatHasField.HasField "handle_owner" Handle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Handle {handle_owner = y1, handle_uses = BG.getField @"handle_uses" x0}
      , BG.getField @"handle_owner" x0
      )

instance ( ty ~ BG.Ptr SquashedAcrossHeaders.A.A
         ) => BG.HasField "handle_owner" (BG.Ptr Handle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"handle_owner")

instance HasCField.HasCField Handle "handle_owner" where

  type CFieldType Handle "handle_owner" =
    BG.Ptr SquashedAcrossHeaders.A.A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @uses@

    __defined at:__ @squashed_across_headers\/private\/handle.h 8:7@

    __exported by:__ @squashed_across_headers.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "handle_uses" Handle ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Handle {handle_uses = y1, handle_owner = BG.getField @"handle_owner" x0}
      , BG.getField @"handle_uses" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "handle_uses" (BG.Ptr Handle) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"handle_uses")

instance HasCField.HasCField Handle "handle_uses" where

  type CFieldType Handle "handle_uses" = BG.CInt

  offset# = \_ -> \_ -> 8
