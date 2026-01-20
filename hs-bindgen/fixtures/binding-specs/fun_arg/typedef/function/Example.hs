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
import qualified M
import qualified Prelude as P
import HsBindgen.Runtime.TypeEquality (TyEq)
import Prelude (IO)

{-| __C declaration:__ @A@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 6:13@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype A = A
  { un_A :: FC.CInt -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

foreign import ccall safe "wrapper" hs_bindgen_0c7d4776a632d026_base ::
     (FC.CInt -> IO FC.CInt)
  -> IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

-- __unique:__ @toA@
hs_bindgen_0c7d4776a632d026 ::
     A
  -> IO (Ptr.FunPtr A)
hs_bindgen_0c7d4776a632d026 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.HasBaseForeignType.castFunPtrFromBaseForeignType (hs_bindgen_0c7d4776a632d026_base (HsBindgen.Runtime.HasBaseForeignType.toBaseForeignType fun0))

foreign import ccall safe "dynamic" hs_bindgen_0cf9a6d50f563441_base ::
     Ptr.FunPtr (FC.CInt -> IO FC.CInt)
  -> FC.CInt -> IO FC.CInt

-- __unique:__ @fromA@
hs_bindgen_0cf9a6d50f563441 ::
     Ptr.FunPtr A
  -> A
hs_bindgen_0cf9a6d50f563441 =
  \funPtr0 ->
    HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType (hs_bindgen_0cf9a6d50f563441_base (HsBindgen.Runtime.HasBaseForeignType.castFunPtrToBaseForeignType funPtr0))

instance HsBindgen.Runtime.FunPtr.ToFunPtr A where

  toFunPtr = hs_bindgen_0c7d4776a632d026

instance HsBindgen.Runtime.FunPtr.FromFunPtr A where

  fromFunPtr = hs_bindgen_0cf9a6d50f563441

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType A) "un_A")
         ) => GHC.Records.HasField "un_A" (Ptr.Ptr A) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_A")

instance HsBindgen.Runtime.HasCField.HasCField A "un_A" where

  type CFieldType A "un_A" = FC.CInt -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @B@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 7:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype B = B
  { un_B :: A
  }
  deriving newtype (HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType B) "un_B")
         ) => GHC.Records.HasField "un_B" (Ptr.Ptr B) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_B")

instance HsBindgen.Runtime.HasCField.HasCField B "un_B" where

  type CFieldType B "un_B" = A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @E@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 19:11@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
newtype E = E
  { un_E :: M.C
  }

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "un_E")
         ) => GHC.Records.HasField "un_E" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_E")

instance HsBindgen.Runtime.HasCField.HasCField E "un_E" where

  type CFieldType E "un_E" = M.C

  offset# = \_ -> \_ -> 0
