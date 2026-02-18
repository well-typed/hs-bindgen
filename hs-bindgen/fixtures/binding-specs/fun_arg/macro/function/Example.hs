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

{-| __C declaration:__ @MyFunction@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype MyFunction = MyFunction
  { unwrapMyFunction :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_4e8a459829b269e0_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toMyFunction@
hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (RIP.FunPtr MyFunction)
hs_bindgen_4e8a459829b269e0 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_4e8a459829b269e0_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_bb71f7e730356103_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromMyFunction@
hs_bindgen_bb71f7e730356103 ::
     RIP.FunPtr MyFunction
  -> MyFunction
hs_bindgen_bb71f7e730356103 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_bb71f7e730356103_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr MyFunction where

  toFunPtr = hs_bindgen_4e8a459829b269e0

instance RIP.FromFunPtr MyFunction where

  fromFunPtr = hs_bindgen_bb71f7e730356103

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapMyFunction" (RIP.Ptr MyFunction) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyFunction")

instance HasCField.HasCField MyFunction "unwrapMyFunction" where

  type CFieldType MyFunction "unwrapMyFunction" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype A = A
  { unwrapA :: MyFunction
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance ( ((~) ty) MyFunction
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyFunction

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance (((~) ty) A) => RIP.HasField "unwrapB" (RIP.Ptr B) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapB")

instance HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
