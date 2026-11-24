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

module ExternalCallbacks.Handlers
    ( ExternalCallbacks.Handlers.Handlers(..)
    )
  where

import qualified GeneratedSpecs.Same_header
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure, type (~))

{-| __C declaration:__ @struct handlers@

    __defined at:__ @external_callbacks\/handlers.h 12:8@

    __exported by:__ @external_callbacks.h@
-}
data Handlers = Handlers
  { handlers_on_open :: BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
    {- ^ __C declaration:__ @on_open@

         __defined at:__ @external_callbacks\/handlers.h 13:10@

         __exported by:__ @external_callbacks.h@
    -}
  , handlers_on_close :: BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
    {- ^ __C declaration:__ @on_close@

         __defined at:__ @external_callbacks\/handlers.h 14:10@

         __exported by:__ @external_callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Handlers where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Handlers where

  readRaw =
    \ptr0 ->
          pure Handlers
      <*> HasCField.readRaw (BG.Proxy @"handlers_on_open") ptr0
      <*> HasCField.readRaw (BG.Proxy @"handlers_on_close") ptr0

instance Marshal.WriteRaw Handlers where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Handlers handlers_on_open2 handlers_on_close3 ->
               HasCField.writeRaw (BG.Proxy @"handlers_on_open") ptr0 handlers_on_open2
            >> HasCField.writeRaw (BG.Proxy @"handlers_on_close") ptr0 handlers_on_close3

deriving via Marshal.EquivStorable Handlers instance BG.Storable Handlers

deriving via Struct.IsStructViaReadRaw Handlers instance Struct.IsStruct Handlers

{-| __C declaration:__ @on_open@

    __defined at:__ @external_callbacks\/handlers.h 13:10@

    __exported by:__ @external_callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
         ) => BG.CompatHasField.HasField "handlers_on_open" Handlers ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Handlers {handlers_on_open = y1, handlers_on_close = BG.getField @"handlers_on_close" x0}
      , BG.getField @"handlers_on_open" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
         ) => BG.HasField "handlers_on_open" (BG.Ptr Handlers) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"handlers_on_open")

instance HasCField.HasCField Handlers "handlers_on_open" where

  type CFieldType Handlers "handlers_on_open" =
    BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @on_close@

    __defined at:__ @external_callbacks\/handlers.h 14:10@

    __exported by:__ @external_callbacks.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
         ) => BG.CompatHasField.HasField "handlers_on_close" Handlers ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Handlers {handlers_on_close = y1, handlers_on_open = BG.getField @"handlers_on_open" x0}
      , BG.getField @"handlers_on_close" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())
         ) => BG.HasField "handlers_on_close" (BG.Ptr Handlers) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"handlers_on_close")

instance HasCField.HasCField Handlers "handlers_on_close" where

  type CFieldType Handlers "handlers_on_close" =
    BG.FunPtr (BG.Ptr Handlers -> BG.Ptr GeneratedSpecs.Same_header.Same_header -> IO ())

  offset# = \_ -> \_ -> 8
