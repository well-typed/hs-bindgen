{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toFoo_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b5a7b5e83ffee6b4_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toFoo_Aux@
hs_bindgen_b5a7b5e83ffee6b4 ::
     Foo_Aux
  -> IO (BG.FunPtr Foo_Aux)
hs_bindgen_b5a7b5e83ffee6b4 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b5a7b5e83ffee6b4_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrap" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromFoo_Aux@
foreign import ccall safe "dynamic" hs_bindgen_223d08172bb37c01_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromFoo_Aux@
hs_bindgen_223d08172bb37c01 ::
     BG.FunPtr Foo_Aux
  -> Foo_Aux
hs_bindgen_223d08172bb37c01 =
  \funPtr0 ->
    Foo_Aux (\x1 ->
               fmap BG.fromFFIType (hs_bindgen_223d08172bb37c01_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
