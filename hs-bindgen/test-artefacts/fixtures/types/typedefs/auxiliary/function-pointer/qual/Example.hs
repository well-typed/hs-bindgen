{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.T_Aux(..)
    , Example.T(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'T'

    __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 1:23@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype T_Aux = T_Aux
  { unwrapT_Aux :: RIP.CInt -> IO ()
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

-- __unique:__ @toT_Aux@
foreign import ccall safe "wrapper" hs_bindgen_5927fedc3abfa99c_base ::
     (RIP.Int32 -> IO ())
  -> IO (RIP.FunPtr (RIP.Int32 -> IO ()))

-- __unique:__ @toT_Aux@
hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (RIP.FunPtr T_Aux)
hs_bindgen_5927fedc3abfa99c =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_5927fedc3abfa99c_base (RIP.toFFIType fun0))

-- __unique:__ @fromT_Aux@
foreign import ccall safe "dynamic" hs_bindgen_281617c90fa9307a_base ::
     RIP.FunPtr (RIP.Int32 -> IO ())
  -> RIP.Int32 -> IO ()

-- __unique:__ @fromT_Aux@
hs_bindgen_281617c90fa9307a ::
     RIP.FunPtr T_Aux
  -> T_Aux
hs_bindgen_281617c90fa9307a =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_281617c90fa9307a_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr T_Aux where

  toFunPtr = hs_bindgen_5927fedc3abfa99c

instance RIP.FromFunPtr T_Aux where

  fromFunPtr = hs_bindgen_281617c90fa9307a

instance ( ty ~ (RIP.CInt -> IO ())
         ) => RIP.HasField "unwrapT_Aux" (RIP.Ptr T_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapT_Aux")

instance HasCField.HasCField T_Aux "unwrapT_Aux" where

  type CFieldType T_Aux "unwrapT_Aux" =
    RIP.CInt -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @T@

    __defined at:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h 1:23@

    __exported by:__ @types\/typedefs\/auxiliary\/function-pointer\/qual.h@
-}
newtype T = T
  { unwrapT :: RIP.FunPtr T_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ RIP.FunPtr T_Aux
         ) => RIP.HasField "unwrapT" (RIP.Ptr T) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapT")

instance HasCField.HasCField T "unwrapT" where

  type CFieldType T "unwrapT" = RIP.FunPtr T_Aux

  offset# = \_ -> \_ -> 0
