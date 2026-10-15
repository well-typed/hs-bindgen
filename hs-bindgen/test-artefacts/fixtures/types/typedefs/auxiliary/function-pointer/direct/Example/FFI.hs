{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toT@
foreign import ccall safe "wrapper" hs_bindgen_b8534912f6256492_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toT@
hs_bindgen_b8534912f6256492 ::
     T
  -> IO (BG.FunPtr T)
hs_bindgen_b8534912f6256492 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b8534912f6256492_base (\x1 ->
                                                            BG.getField @"unwrapT" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromT@
foreign import ccall safe "dynamic" hs_bindgen_8f830eadb43a6fe4_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromT@
hs_bindgen_8f830eadb43a6fe4 ::
     BG.FunPtr T
  -> T
hs_bindgen_8f830eadb43a6fe4 =
  \funPtr0 ->
    T (\x1 ->
         hs_bindgen_8f830eadb43a6fe4_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))
