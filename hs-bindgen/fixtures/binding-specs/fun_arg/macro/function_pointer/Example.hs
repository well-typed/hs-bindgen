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
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified Prelude as P
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, IO, Ord, Show)

{-| Auxiliary type used by 'MyFunctionPointer'

__C declaration:__ @MyFunctionPointer@

__defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 4:15@

__exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer_Aux = MyFunctionPointer_Aux
  { un_MyFunctionPointer_Aux :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_47dfd04698dd2e6f_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

-- __unique:__ @toMyFunctionPointer_Aux@
hs_bindgen_47dfd04698dd2e6f ::
     MyFunctionPointer_Aux
  -> IO (Ptr.FunPtr MyFunctionPointer_Aux)
hs_bindgen_47dfd04698dd2e6f =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_47dfd04698dd2e6f_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_5738272f94a589e2_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

-- __unique:__ @fromMyFunctionPointer_Aux@
hs_bindgen_5738272f94a589e2 ::
     Ptr.FunPtr MyFunctionPointer_Aux
  -> MyFunctionPointer_Aux
hs_bindgen_5738272f94a589e2 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_5738272f94a589e2_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr MyFunctionPointer_Aux where

  toFunPtr = hs_bindgen_47dfd04698dd2e6f

instance HsBindgen.Runtime.FunPtr.FromFunPtr MyFunctionPointer_Aux where

  fromFunPtr = hs_bindgen_5738272f94a589e2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyFunctionPointer_Aux) "un_MyFunctionPointer_Aux")
         ) => GHC.Records.HasField "un_MyFunctionPointer_Aux" (Ptr.Ptr MyFunctionPointer_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyFunctionPointer_Aux")

instance HsBindgen.Runtime.HasCField.HasCField MyFunctionPointer_Aux "un_MyFunctionPointer_Aux" where

  type CFieldType MyFunctionPointer_Aux "un_MyFunctionPointer_Aux" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @MyFunctionPointer@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 4:15@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype MyFunctionPointer = MyFunctionPointer
  { un_MyFunctionPointer :: Ptr.FunPtr MyFunctionPointer_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyFunctionPointer) "un_MyFunctionPointer")
         ) => GHC.Records.HasField "un_MyFunctionPointer" (Ptr.Ptr MyFunctionPointer) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyFunctionPointer")

instance HsBindgen.Runtime.HasCField.HasCField MyFunctionPointer "un_MyFunctionPointer" where

  type CFieldType MyFunctionPointer "un_MyFunctionPointer" =
    Ptr.FunPtr MyFunctionPointer_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 7:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype A = A
  { un_A :: MyFunctionPointer
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 8:9@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)
