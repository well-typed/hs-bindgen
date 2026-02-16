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
import qualified M

{-| Auxiliary type used by 'A'

__C declaration:__ @A@

__defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

__exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A_Aux = A_Aux
  { unwrapA_Aux :: RIP.CInt -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_1cabb32c661d9a0e_base ::
     (RIP.Int32 -> IO RIP.Int32)
  -> IO (RIP.FunPtr (RIP.Int32 -> IO RIP.Int32))

-- __unique:__ @toA_Aux@
hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (RIP.FunPtr A_Aux)
hs_bindgen_1cabb32c661d9a0e =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_1cabb32c661d9a0e_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_cdb12400c6863f15_base ::
     RIP.FunPtr (RIP.Int32 -> IO RIP.Int32)
  -> RIP.Int32 -> IO RIP.Int32

-- __unique:__ @fromA_Aux@
hs_bindgen_cdb12400c6863f15 ::
     RIP.FunPtr A_Aux
  -> A_Aux
hs_bindgen_cdb12400c6863f15 =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_cdb12400c6863f15_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr A_Aux where

  toFunPtr = hs_bindgen_1cabb32c661d9a0e

instance RIP.FromFunPtr A_Aux where

  fromFunPtr = hs_bindgen_cdb12400c6863f15

instance ( ((~) ty) (RIP.CInt -> IO RIP.CInt)
         ) => RIP.HasField "unwrapA_Aux" (RIP.Ptr A_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapA_Aux")

instance HasCField.HasCField A_Aux "unwrapA_Aux" where

  type CFieldType A_Aux "unwrapA_Aux" =
    RIP.CInt -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A = A
  { unwrapA :: RIP.FunPtr A_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr A_Aux)
         ) => RIP.HasField "unwrapA" (RIP.Ptr A) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapA")

instance HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = RIP.FunPtr A_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
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

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

instance (((~) ty) M.C) => RIP.HasField "unwrapE" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrapE")

instance HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
