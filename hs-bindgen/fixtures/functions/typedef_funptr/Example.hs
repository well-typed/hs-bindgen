{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'RunDriver'

__C declaration:__ @RunDriver@

__defined at:__ @functions\/typedef_funptr.h 12:15@

__exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver_Aux = RunDriver_Aux
  { unwrapRunDriver_Aux :: (RIP.Ptr Driver) -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (RIP.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d86ecf261d7044c6_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> (RIP.Ptr RIP.Void) -> IO RIP.Int32

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     RIP.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr RunDriver_Aux where

  toFunPtr = hs_bindgen_d86ecf261d7044c6

instance RIP.FromFunPtr RunDriver_Aux where

  fromFunPtr = hs_bindgen_6520ae39b50ffb4e

instance ( ((~) ty) ((RIP.Ptr Driver) -> IO RIP.CInt)
         ) => RIP.HasField "unwrapRunDriver_Aux" (RIP.Ptr RunDriver_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapRunDriver_Aux")

instance HasCField.HasCField RunDriver_Aux "unwrapRunDriver_Aux" where

  type CFieldType RunDriver_Aux "unwrapRunDriver_Aux" =
    (RIP.Ptr Driver) -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RunDriver@

    __defined at:__ @functions\/typedef_funptr.h 12:15@

    __exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver = RunDriver
  { unwrapRunDriver :: RIP.FunPtr RunDriver_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr RunDriver_Aux)
         ) => RIP.HasField "unwrapRunDriver" (RIP.Ptr RunDriver) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapRunDriver")

instance HasCField.HasCField RunDriver "unwrapRunDriver" where

  type CFieldType RunDriver "unwrapRunDriver" =
    RIP.FunPtr RunDriver_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Driver@

    __defined at:__ @functions\/typedef_funptr.h 13:8@

    __exported by:__ @functions\/typedef_funptr.h@
-}
data Driver = Driver
  { driver_run :: RunDriver
    {- ^ __C declaration:__ @run@

         __defined at:__ @functions\/typedef_funptr.h 14:13@

         __exported by:__ @functions\/typedef_funptr.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Driver where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Driver where

  readRaw =
    \ptr0 ->
          pure Driver
      <*> HasCField.readRaw (RIP.Proxy @"driver_run") ptr0

instance Marshal.WriteRaw Driver where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Driver driver_run2 ->
            HasCField.writeRaw (RIP.Proxy @"driver_run") ptr0 driver_run2

deriving via Marshal.EquivStorable Driver instance RIP.Storable Driver

instance HasCField.HasCField Driver "driver_run" where

  type CFieldType Driver "driver_run" = RunDriver

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RunDriver
         ) => RIP.HasField "driver_run" (RIP.Ptr Driver) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"driver_run")

{-| __C declaration:__ @struct Bare@

    __defined at:__ @functions\/typedef_funptr.h 20:8@

    __exported by:__ @functions\/typedef_funptr.h@
-}
data Bare = Bare
  { bare_callback :: RIP.FunPtr (RIP.CInt -> IO ())
    {- ^ __C declaration:__ @callback@

         __defined at:__ @functions\/typedef_funptr.h 21:10@

         __exported by:__ @functions\/typedef_funptr.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bare where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bare where

  readRaw =
    \ptr0 ->
          pure Bare
      <*> HasCField.readRaw (RIP.Proxy @"bare_callback") ptr0

instance Marshal.WriteRaw Bare where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bare bare_callback2 ->
            HasCField.writeRaw (RIP.Proxy @"bare_callback") ptr0 bare_callback2

deriving via Marshal.EquivStorable Bare instance RIP.Storable Bare

instance HasCField.HasCField Bare "bare_callback" where

  type CFieldType Bare "bare_callback" =
    RIP.FunPtr (RIP.CInt -> IO ())

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.FunPtr (RIP.CInt -> IO ()))
         ) => RIP.HasField "bare_callback" (RIP.Ptr Bare) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"bare_callback")
