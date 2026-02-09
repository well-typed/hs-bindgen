{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import Data.Void (Void)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure)

{-| Auxiliary type used by 'RunDriver'

__C declaration:__ @RunDriver@

__defined at:__ @functions\/typedef_funptr.h 12:15@

__exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver_Aux = RunDriver_Aux
  { unwrapRunDriver_Aux :: (Ptr.Ptr Driver) -> IO FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (Ptr.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_d86ecf261d7044c6_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> (Ptr.Ptr Void) -> IO GHC.Int.Int32

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     Ptr.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr RunDriver_Aux where

  toFunPtr = hs_bindgen_d86ecf261d7044c6

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr RunDriver_Aux where

  fromFunPtr = hs_bindgen_6520ae39b50ffb4e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType RunDriver_Aux) "unwrapRunDriver_Aux")
         ) => GHC.Records.HasField "unwrapRunDriver_Aux" (Ptr.Ptr RunDriver_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapRunDriver_Aux")

instance HsBindgen.Runtime.HasCField.HasCField RunDriver_Aux "unwrapRunDriver_Aux" where

  type CFieldType RunDriver_Aux "unwrapRunDriver_Aux" =
    (Ptr.Ptr Driver) -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RunDriver@

    __defined at:__ @functions\/typedef_funptr.h 12:15@

    __exported by:__ @functions\/typedef_funptr.h@
-}
newtype RunDriver = RunDriver
  { unwrapRunDriver :: Ptr.FunPtr RunDriver_Aux
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType RunDriver) "unwrapRunDriver")
         ) => GHC.Records.HasField "unwrapRunDriver" (Ptr.Ptr RunDriver) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapRunDriver")

instance HsBindgen.Runtime.HasCField.HasCField RunDriver "unwrapRunDriver" where

  type CFieldType RunDriver "unwrapRunDriver" =
    Ptr.FunPtr RunDriver_Aux

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
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Driver where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Driver where

  readRaw =
    \ptr0 ->
          pure Driver
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"driver_run") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Driver where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Driver driver_run2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"driver_run") ptr0 driver_run2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Driver instance F.Storable Driver

instance HsBindgen.Runtime.HasCField.HasCField Driver "driver_run" where

  type CFieldType Driver "driver_run" = RunDriver

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Driver) "driver_run")
         ) => GHC.Records.HasField "driver_run" (Ptr.Ptr Driver) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"driver_run")

{-| __C declaration:__ @struct Bare@

    __defined at:__ @functions\/typedef_funptr.h 20:8@

    __exported by:__ @functions\/typedef_funptr.h@
-}
data Bare = Bare
  { bare_callback :: Ptr.FunPtr (FC.CInt -> IO ())
    {- ^ __C declaration:__ @callback@

         __defined at:__ @functions\/typedef_funptr.h 21:10@

         __exported by:__ @functions\/typedef_funptr.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Bare where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Bare where

  readRaw =
    \ptr0 ->
          pure Bare
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"bare_callback") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Bare where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bare bare_callback2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"bare_callback") ptr0 bare_callback2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Bare instance F.Storable Bare

instance HsBindgen.Runtime.HasCField.HasCField Bare "bare_callback" where

  type CFieldType Bare "bare_callback" =
    Ptr.FunPtr (FC.CInt -> IO ())

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Bare) "bare_callback")
         ) => GHC.Records.HasField "bare_callback" (Ptr.Ptr Bare) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"bare_callback")
