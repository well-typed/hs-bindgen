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
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import qualified M
import qualified Prelude as P
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (Eq, IO, Ord, Show)

{-| Auxiliary type used by 'A'

__C declaration:__ @A@

__defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

__exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A_Aux = A_Aux
  { unwrapA_Aux :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_1cabb32c661d9a0e_base ::
     (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32))

-- __unique:__ @toA_Aux@
hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (Ptr.FunPtr A_Aux)
hs_bindgen_1cabb32c661d9a0e =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasFFIType.castFunPtrFromFFIType (hs_bindgen_1cabb32c661d9a0e_base (HsBindgen.Runtime.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_cdb12400c6863f15_base ::
     Ptr.FunPtr (GHC.Int.Int32 -> IO GHC.Int.Int32)
  -> GHC.Int.Int32 -> IO GHC.Int.Int32

-- __unique:__ @fromA_Aux@
hs_bindgen_cdb12400c6863f15 ::
     Ptr.FunPtr A_Aux
  -> A_Aux
hs_bindgen_cdb12400c6863f15 =
  \funPtr0 ->
    HsBindgen.Runtime.HasFFIType.fromFFIType (hs_bindgen_cdb12400c6863f15_base (HsBindgen.Runtime.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr A_Aux where

  toFunPtr = hs_bindgen_1cabb32c661d9a0e

instance HsBindgen.Runtime.FunPtr.FromFunPtr A_Aux where

  fromFunPtr = hs_bindgen_cdb12400c6863f15

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A_Aux) "unwrapA_Aux")
         ) => GHC.Records.HasField "unwrapA_Aux" (Ptr.Ptr A_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA_Aux")

instance HsBindgen.Runtime.HasCField.HasCField A_Aux "unwrapA_Aux" where

  type CFieldType A_Aux "unwrapA_Aux" =
    FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 6:15@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype A = A
  { unwrapA :: Ptr.FunPtr A_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "unwrapA")
         ) => GHC.Records.HasField "unwrapA" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapA")

instance HsBindgen.Runtime.HasCField.HasCField A "unwrapA" where

  type CFieldType A "unwrapA" = Ptr.FunPtr A_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype B = B
  { unwrapB :: A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "unwrapB")
         ) => GHC.Records.HasField "unwrapB" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapB")

instance HsBindgen.Runtime.HasCField.HasCField B "unwrapB" where

  type CFieldType B "unwrapB" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function_pointer.h@
-}
newtype E = E
  { unwrapE :: M.C
  }
  deriving newtype (HsBindgen.Runtime.HasFFIType.HasFFIType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "unwrapE")
         ) => GHC.Records.HasField "unwrapE" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapE")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrapE" where

  type CFieldType E "unwrapE" = M.C

  offset# = \_ -> \_ -> 0
