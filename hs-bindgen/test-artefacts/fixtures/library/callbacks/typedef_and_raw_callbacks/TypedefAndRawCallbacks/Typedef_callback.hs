{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TypedefAndRawCallbacks.Typedef_callback
    ( TypedefAndRawCallbacks.Typedef_callback.Event(..)
    , TypedefAndRawCallbacks.Typedef_callback.Typedef_callback_Aux(..)
    , TypedefAndRawCallbacks.Typedef_callback.Typedef_callback(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), Eq, IO, Int, Ord, Show, fmap, pure, type (~))

{-| __C declaration:__ @struct event@

    __defined at:__ @typedef_and_raw_callbacks\/typedef_callback.h 4:8@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
data Event = Event
  { event_code :: BG.CInt
    {- ^ __C declaration:__ @code@

         __defined at:__ @typedef_and_raw_callbacks\/typedef_callback.h 5:7@

         __exported by:__ @typedef_and_raw_callbacks.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Event where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Event where

  readRaw =
    \ptr0 ->
          pure Event
      <*> HasCField.readRaw (BG.Proxy @"event_code") ptr0

instance Marshal.WriteRaw Event where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Event event_code2 ->
            HasCField.writeRaw (BG.Proxy @"event_code") ptr0 event_code2

deriving via Marshal.EquivStorable Event instance BG.Storable Event

deriving via Struct.IsStructViaReadRaw Event instance Struct.IsStruct Event

{-| __C declaration:__ @code@

    __defined at:__ @typedef_and_raw_callbacks\/typedef_callback.h 5:7@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "event_code" Event ty where

  hasField =
    \x0 ->
      (\y1 ->
         Event {event_code = y1}, BG.getField @"event_code" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "event_code" (BG.Ptr Event) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"event_code")

instance HasCField.HasCField Event "event_code" where

  type CFieldType Event "event_code" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| Auxiliary type used by 'Typedef_callback'

    __C declaration:__ @typedef_callback@

    __defined at:__ @typedef_and_raw_callbacks\/typedef_callback.h 10:16@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
newtype Typedef_callback_Aux = Typedef_callback_Aux
  { unwrapTypedef_callback_Aux :: PtrConst.PtrConst Event -> BG.Ptr BG.Void -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (PtrConst.PtrConst Event -> BG.Ptr BG.Void -> IO ())
         ) => BG.CompatHasField.HasField "unwrapTypedef_callback_Aux" Typedef_callback_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Typedef_callback_Aux {unwrapTypedef_callback_Aux = y1}
      , BG.getField @"unwrapTypedef_callback_Aux" x0
      )

instance ( ty ~ (PtrConst.PtrConst Event -> BG.Ptr BG.Void -> IO ())
         ) => BG.HasField "unwrapTypedef_callback_Aux" (BG.Ptr Typedef_callback_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypedef_callback_Aux")

instance HasCField.HasCField Typedef_callback_Aux "unwrapTypedef_callback_Aux" where

  type CFieldType Typedef_callback_Aux "unwrapTypedef_callback_Aux" =
    PtrConst.PtrConst Event -> BG.Ptr BG.Void -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @typedef_callback@

    __defined at:__ @typedef_and_raw_callbacks\/typedef_callback.h 10:16@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
newtype Typedef_callback = Typedef_callback
  { unwrapTypedef_callback :: BG.FunPtr Typedef_callback_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Typedef_callback_Aux
         ) => BG.CompatHasField.HasField "unwrapTypedef_callback" Typedef_callback ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Typedef_callback {unwrapTypedef_callback = y1}
      , BG.getField @"unwrapTypedef_callback" x0
      )

instance ( ty ~ BG.FunPtr Typedef_callback_Aux
         ) => BG.HasField "unwrapTypedef_callback" (BG.Ptr Typedef_callback) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapTypedef_callback")

instance HasCField.HasCField Typedef_callback "unwrapTypedef_callback" where

  type CFieldType Typedef_callback "unwrapTypedef_callback" =
    BG.FunPtr Typedef_callback_Aux

  offset# = \_ -> \_ -> 0

-- __unique:__ @toTypedef_callback_Aux@
foreign import ccall safe "wrapper" hs_bindgen_9e597cf3bc0f375d_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @toTypedef_callback_Aux@
hs_bindgen_9e597cf3bc0f375d ::
     Typedef_callback_Aux
  -> IO (BG.FunPtr Typedef_callback_Aux)
hs_bindgen_9e597cf3bc0f375d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_9e597cf3bc0f375d_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapTypedef_callback_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromTypedef_callback_Aux@
foreign import ccall safe "dynamic" hs_bindgen_f78eba5a3cd8d3f1_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @fromTypedef_callback_Aux@
hs_bindgen_f78eba5a3cd8d3f1 ::
     BG.FunPtr Typedef_callback_Aux
  -> Typedef_callback_Aux
hs_bindgen_f78eba5a3cd8d3f1 =
  \funPtr0 ->
    Typedef_callback_Aux (\x1 ->
                            \x2 ->
                              hs_bindgen_f78eba5a3cd8d3f1_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

instance BG.ToFunPtr Typedef_callback_Aux where

  toFunPtr = hs_bindgen_9e597cf3bc0f375d

instance BG.FromFunPtr Typedef_callback_Aux where

  fromFunPtr = hs_bindgen_f78eba5a3cd8d3f1
