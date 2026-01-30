{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified Prelude as P
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude (IO)

{-| __C declaration:__ @MyFunction@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype MyFunction = MyFunction
  { unwrapMyFunction :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_4e8a459829b269e0_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toMyFunction@
hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (Ptr.FunPtr MyFunction)
hs_bindgen_4e8a459829b269e0 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_4e8a459829b269e0_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_bb71f7e730356103_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromMyFunction@
hs_bindgen_bb71f7e730356103 ::
     Ptr.FunPtr MyFunction
  -> MyFunction
hs_bindgen_bb71f7e730356103 =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_bb71f7e730356103_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr MyFunction where

  toFunPtr = hs_bindgen_4e8a459829b269e0

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr MyFunction where

  fromFunPtr = hs_bindgen_bb71f7e730356103

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyFunction) "unwrapMyFunction")
         ) => GHC.Records.HasField "unwrapMyFunction" (Ptr.Ptr MyFunction) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyFunction")

instance HsBindgen.Runtime.HasCField.HasCField MyFunction "unwrapMyFunction" where

  type CFieldType MyFunction "unwrapMyFunction" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype A = A
  { unwrapA :: MyFunction
  }
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = MyFunction

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "unwrapB")
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0
