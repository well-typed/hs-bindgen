{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.RunDriver_Aux(..)
    , Example.RunDriver(..)
    , Example.Driver(..)
    , Example.Bare(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| Auxiliary type used by 'RunDriver'

    __C declaration:__ @RunDriver@

    __defined at:__ @functions\/typedef_funptr.h 12:15@

    __exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver_Aux = RunDriver_Aux
  { unwrapRunDriver_Aux :: BG.Ptr Driver -> IO BG.CInt
  }
  deriving stock (BG.Generic)
  deriving newtype (BG.HasFFIType)

-- __unique:__ @toRunDriver_Aux@
foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     (BG.Ptr BG.Void -> IO BG.Int32)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> IO BG.Int32))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (BG.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    fmap BG.castFunPtrFromFFIType (hs_bindgen_d86ecf261d7044c6_base (BG.toFFIType fun0))

-- __unique:__ @fromRunDriver_Aux@
foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> IO BG.Int32)
  -> BG.Ptr BG.Void -> IO BG.Int32

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     BG.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    BG.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (BG.castFunPtrToFFIType funPtr0))

instance BG.ToFunPtr RunDriver_Aux where

  toFunPtr = hs_bindgen_d86ecf261d7044c6

instance BG.FromFunPtr RunDriver_Aux where

  fromFunPtr = hs_bindgen_6520ae39b50ffb4e

instance ( ty ~ (BG.Ptr Driver -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapRunDriver_Aux" RunDriver_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> RunDriver_Aux {unwrapRunDriver_Aux = y1}
      , BG.getField @"unwrapRunDriver_Aux" x0
      )

instance ( ty ~ (BG.Ptr Driver -> IO BG.CInt)
         ) => BG.HasField "unwrapRunDriver_Aux" (BG.Ptr RunDriver_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapRunDriver_Aux")

instance HasCField.HasCField RunDriver_Aux "unwrapRunDriver_Aux" where

  type CFieldType RunDriver_Aux "unwrapRunDriver_Aux" =
    BG.Ptr Driver -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RunDriver@

    __defined at:__ @functions\/typedef_funptr.h 12:15@

    __exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver = RunDriver
  { unwrapRunDriver :: BG.FunPtr RunDriver_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr RunDriver_Aux
         ) => BG.CompatHasField.HasField "unwrapRunDriver" RunDriver ty where

  hasField =
    \x0 ->
      (\y1 ->
         RunDriver {unwrapRunDriver = y1}, BG.getField @"unwrapRunDriver" x0)

instance ( ty ~ BG.FunPtr RunDriver_Aux
         ) => BG.HasField "unwrapRunDriver" (BG.Ptr RunDriver) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapRunDriver")

instance HasCField.HasCField RunDriver "unwrapRunDriver" where

  type CFieldType RunDriver "unwrapRunDriver" =
    BG.FunPtr RunDriver_Aux

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
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Driver where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Driver where

  readRaw =
    \ptr0 ->
          pure Driver
      <*> HasCField.readRaw (BG.Proxy @"driver_run") ptr0

instance Marshal.WriteRaw Driver where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Driver driver_run2 ->
            HasCField.writeRaw (BG.Proxy @"driver_run") ptr0 driver_run2

deriving via Marshal.EquivStorable Driver instance BG.Storable Driver

{-| __C declaration:__ @run@

    __defined at:__ @functions\/typedef_funptr.h 14:13@

    __exported by:__ @functions\/typedef_funptr.h@
-}
instance ( ty ~ RunDriver
         ) => BG.CompatHasField.HasField "driver_run" Driver ty where

  hasField =
    \x0 ->
      (\y1 ->
         Driver {driver_run = y1}, BG.getField @"driver_run" x0)

instance ( ty ~ RunDriver
         ) => BG.HasField "driver_run" (BG.Ptr Driver) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"driver_run")

instance HasCField.HasCField Driver "driver_run" where

  type CFieldType Driver "driver_run" = RunDriver

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Bare@

    __defined at:__ @functions\/typedef_funptr.h 20:8@

    __exported by:__ @functions\/typedef_funptr.h@
-}
data Bare = Bare
  { bare_callback :: BG.FunPtr (BG.CInt -> IO ())
    {- ^ __C declaration:__ @callback@

         __defined at:__ @functions\/typedef_funptr.h 21:10@

         __exported by:__ @functions\/typedef_funptr.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bare where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bare where

  readRaw =
    \ptr0 ->
          pure Bare
      <*> HasCField.readRaw (BG.Proxy @"bare_callback") ptr0

instance Marshal.WriteRaw Bare where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bare bare_callback2 ->
            HasCField.writeRaw (BG.Proxy @"bare_callback") ptr0 bare_callback2

deriving via Marshal.EquivStorable Bare instance BG.Storable Bare

{-| __C declaration:__ @callback@

    __defined at:__ @functions\/typedef_funptr.h 21:10@

    __exported by:__ @functions\/typedef_funptr.h@
-}
instance ( ty ~ BG.FunPtr (BG.CInt -> IO ())
         ) => BG.CompatHasField.HasField "bare_callback" Bare ty where

  hasField =
    \x0 ->
      (\y1 ->
         Bare {bare_callback = y1}, BG.getField @"bare_callback" x0)

instance ( ty ~ BG.FunPtr (BG.CInt -> IO ())
         ) => BG.HasField "bare_callback" (BG.Ptr Bare) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"bare_callback")

instance HasCField.HasCField Bare "bare_callback" where

  type CFieldType Bare "bare_callback" =
    BG.FunPtr (BG.CInt -> IO ())

  offset# = \_ -> \_ -> 0
