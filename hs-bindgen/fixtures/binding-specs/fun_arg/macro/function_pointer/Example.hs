{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| Auxiliary type used by 'MyFunctionPointer'

__C declaration:__ @MyFunctionPointer@

__defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 4:15@

__exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer_Aux = MyFunctionPointer_Aux
  { unwrapMyFunctionPointer_Aux :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_47dfd04698dd2e6f_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toMyFunctionPointer_Aux@
hs_bindgen_47dfd04698dd2e6f ::
     MyFunctionPointer_Aux
  -> IO (RIP.FunPtr MyFunctionPointer_Aux)
hs_bindgen_47dfd04698dd2e6f =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_47dfd04698dd2e6f_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_5738272f94a589e2_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromMyFunctionPointer_Aux@
hs_bindgen_5738272f94a589e2 ::
     RIP.FunPtr MyFunctionPointer_Aux
  -> MyFunctionPointer_Aux
hs_bindgen_5738272f94a589e2 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_5738272f94a589e2_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr MyFunctionPointer_Aux where

  toFunPtr = hs_bindgen_47dfd04698dd2e6f

instance RIP.FromFunPtr MyFunctionPointer_Aux where

  fromFunPtr = hs_bindgen_5738272f94a589e2

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapMyFunctionPointer_Aux" (RIP.Ptr MyFunctionPointer_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyFunctionPointer_Aux")

instance HasCField.HasCField MyFunctionPointer_Aux "unwrapMyFunctionPointer_Aux" where

  type CFieldType MyFunctionPointer_Aux "unwrapMyFunctionPointer_Aux" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyFunctionPointer@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 4:15@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer = MyFunctionPointer
  { unwrapMyFunctionPointer :: RIP.FunPtr MyFunctionPointer_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr MyFunctionPointer_Aux)
         ) => RIP.HasField "unwrapMyFunctionPointer" (RIP.Ptr MyFunctionPointer) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyFunctionPointer")

instance HasCField.HasCField MyFunctionPointer "unwrapMyFunctionPointer" where

  type CFieldType MyFunctionPointer "unwrapMyFunctionPointer" =
    RIP.FunPtr MyFunctionPointer_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype A = A
  { unwrapA :: MyFunctionPointer
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) MyFunctionPointer
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyFunctionPointer

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
