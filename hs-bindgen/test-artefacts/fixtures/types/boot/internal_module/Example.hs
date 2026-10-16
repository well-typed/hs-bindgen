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
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.On_event_Aux(..)
    , Example.On_event(..)
    , Example.Buffer_Aux(..)
    , Example.Buffer
    )
  where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure, type (~))
import {-# SOURCE #-} Example.Callback

{-| Auxiliary type used by 'On_event'

    __C declaration:__ @on_event@

    __defined at:__ @types\/boot\/internal_module.h 7:15@

    __exported by:__ @types\/boot\/internal_module.h@
-}
newtype On_event_Aux = On_event_Aux
  { unwrapOn_event_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)

instance BG.ToFunPtr On_event_Aux where

  toFunPtr = hs_bindgen_70c2110bb36ed51a

instance BG.FromFunPtr On_event_Aux where

  fromFunPtr = hs_bindgen_a7705d093ccaa37a

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapOn_event_Aux" On_event_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> On_event_Aux {unwrapOn_event_Aux = y1}
      , BG.getField @"unwrapOn_event_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapOn_event_Aux" (BG.Ptr On_event_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_event_Aux")

instance HasCField.HasCField On_event_Aux "unwrapOn_event_Aux" where

  type CFieldType On_event_Aux "unwrapOn_event_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @on_event@

    __defined at:__ @types\/boot\/internal_module.h 7:15@

    __exported by:__ @types\/boot\/internal_module.h@
-}
newtype On_event = On_event
  { unwrapOn_event :: BG.FunPtr On_event_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr On_event_Aux
         ) => BG.CompatHasField.HasField "unwrapOn_event" On_event ty where

  hasField =
    \x0 ->
      (\y1 ->
         On_event {unwrapOn_event = y1}, BG.getField @"unwrapOn_event" x0)

instance ( ty ~ BG.FunPtr On_event_Aux
         ) => BG.HasField "unwrapOn_event" (BG.Ptr On_event) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_event")

instance HasCField.HasCField On_event "unwrapOn_event" where

  type CFieldType On_event "unwrapOn_event" =
    BG.FunPtr On_event_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct buffer@

    __defined at:__ @types\/boot\/internal_module.h 11:8@

    __exported by:__ @types\/boot\/internal_module.h@
-}
data Buffer_Aux = Buffer
  { buffer_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @types\/boot\/internal_module.h 12:7@

         __exported by:__ @types\/boot\/internal_module.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Buffer_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Buffer_Aux where

  readRaw =
    \ptr0 ->
          pure Buffer
      <*> HasCField.readRaw (BG.Proxy @"buffer_len") ptr0

instance Marshal.WriteRaw Buffer_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Buffer buffer_len2 ->
            HasCField.writeRaw (BG.Proxy @"buffer_len") ptr0 buffer_len2

deriving via Marshal.EquivStorable Buffer_Aux instance BG.Storable Buffer_Aux

deriving via Struct.IsStructViaReadRaw Buffer_Aux instance Struct.IsStruct Buffer_Aux

{-| __C declaration:__ @len@

    __defined at:__ @types\/boot\/internal_module.h 12:7@

    __exported by:__ @types\/boot\/internal_module.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "buffer_len" Buffer_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Buffer {buffer_len = y1}, BG.getField @"buffer_len" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "buffer_len" (BG.Ptr Buffer_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"buffer_len")

instance HasCField.HasCField Buffer_Aux "buffer_len" where

  type CFieldType Buffer_Aux "buffer_len" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset BG.CChar Buffer_Aux where

  offset = \_proxy0 -> 4

{-| __C declaration:__ @struct buffer@

    __defined at:__ @types\/boot\/internal_module.h 11:8@

    __exported by:__ @types\/boot\/internal_module.h@
-}
type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux
