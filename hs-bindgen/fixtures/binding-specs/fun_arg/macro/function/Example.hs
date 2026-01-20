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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Prelude as P
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (IO)

{-| __C declaration:__ @MyFunction@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 4:13@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype MyFunction = MyFunction
  { un_MyFunction :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_4e8a459829b269e0_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

-- __unique:__ @toMyFunction@
hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (Ptr.FunPtr MyFunction)
hs_bindgen_4e8a459829b269e0 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_4e8a459829b269e0_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_bb71f7e730356103_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

-- __unique:__ @fromMyFunction@
hs_bindgen_bb71f7e730356103 ::
     Ptr.FunPtr MyFunction
  -> MyFunction
hs_bindgen_bb71f7e730356103 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_bb71f7e730356103_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr MyFunction where

  toFunPtr = hs_bindgen_4e8a459829b269e0

instance HsBindgen.Runtime.FunPtr.FromFunPtr MyFunction where

  fromFunPtr = hs_bindgen_bb71f7e730356103

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyFunction) "un_MyFunction")
         ) => GHC.Records.HasField "un_MyFunction" (Ptr.Ptr MyFunction) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyFunction")

instance HsBindgen.Runtime.HasCField.HasCField MyFunction "un_MyFunction" where

  type CFieldType MyFunction "un_MyFunction" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype A = A
  { un_A :: MyFunction
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)
