{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toA@
foreign import ccall safe "wrapper" hs_bindgen_0c7d4776a632d026_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toA@
hs_bindgen_0c7d4776a632d026 ::
     A
  -> IO (BG.FunPtr A)
hs_bindgen_0c7d4776a632d026 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_0c7d4776a632d026_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapA" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromA@
foreign import ccall safe "dynamic" hs_bindgen_0cf9a6d50f563441_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromA@
hs_bindgen_0cf9a6d50f563441 ::
     BG.FunPtr A
  -> A
hs_bindgen_0cf9a6d50f563441 =
  \funPtr0 ->
    A (\x1 ->
         fmap BG.fromFFIType (hs_bindgen_0cf9a6d50f563441_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
