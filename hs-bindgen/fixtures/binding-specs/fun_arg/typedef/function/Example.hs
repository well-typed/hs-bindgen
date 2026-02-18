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
import qualified M

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype A = A
  { unwrapA :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_0c7d4776a632d026_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toA@
hs_bindgen_0c7d4776a632d026 ::
     A
  -> IO (RIP.FunPtr A)
hs_bindgen_0c7d4776a632d026 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_0c7d4776a632d026_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0cf9a6d50f563441_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromA@
hs_bindgen_0cf9a6d50f563441 ::
     RIP.FunPtr A
  -> A
hs_bindgen_0cf9a6d50f563441 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_0cf9a6d50f563441_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr A where

  toFunPtr = hs_bindgen_0c7d4776a632d026

instance RIP.FromFunPtr A where

  fromFunPtr = hs_bindgen_0cf9a6d50f563441

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
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

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (RIP.Generic)

instance (((~) ty) M.C) => RIP.HasField "unwrapE" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
