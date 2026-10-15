{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toMyFunction@
foreign import ccall safe "wrapper" hs_bindgen_4e8a459829b269e0_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toMyFunction@
hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (BG.FunPtr MyFunction)
hs_bindgen_4e8a459829b269e0 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_4e8a459829b269e0_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapMyFunction" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromMyFunction@
foreign import ccall safe "dynamic" hs_bindgen_bb71f7e730356103_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromMyFunction@
hs_bindgen_bb71f7e730356103 ::
     BG.FunPtr MyFunction
  -> MyFunction
hs_bindgen_bb71f7e730356103 =
  \funPtr0 ->
    MyFunction (\x1 ->
                  fmap BG.fromFFIType (hs_bindgen_bb71f7e730356103_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))
