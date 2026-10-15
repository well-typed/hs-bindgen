{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.CShort -> IO I)@
foreign import ccall safe "wrapper" hs_bindgen_03e9923d8d301cca_base ::
     (BG.CShort -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))

-- __unique:__ @instance ToFunPtr (BG.CShort -> IO I)@
hs_bindgen_03e9923d8d301cca ::
     (BG.CShort -> IO I)
  -> IO (BG.FunPtr (BG.CShort -> IO I))
hs_bindgen_03e9923d8d301cca =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_03e9923d8d301cca_base (\x1 ->
                                                            fmap BG.toFFIType (fun0 (BG.fromFFIType x1))))

-- __unique:__ @instance FromFunPtr (BG.CShort -> IO I)@
foreign import ccall safe "dynamic" hs_bindgen_ee10a61bcb1a5d39_base ::
     BG.FunPtr (BG.CShort -> IO BG.CInt)
  -> BG.CShort -> IO BG.CInt

-- __unique:__ @instance FromFunPtr (BG.CShort -> IO I)@
hs_bindgen_ee10a61bcb1a5d39 ::
     BG.FunPtr (BG.CShort -> IO I)
  -> BG.CShort -> IO I
hs_bindgen_ee10a61bcb1a5d39 =
  \funPtr0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_ee10a61bcb1a5d39_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))

-- __unique:__ @instance ToFunPtr (S -> IO BG.CInt)@
foreign import ccall safe "wrapper" hs_bindgen_140ec1313a27c405_base ::
     (BG.CShort -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))

-- __unique:__ @instance ToFunPtr (S -> IO BG.CInt)@
hs_bindgen_140ec1313a27c405 ::
     (S -> IO BG.CInt)
  -> IO (BG.FunPtr (S -> IO BG.CInt))
hs_bindgen_140ec1313a27c405 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_140ec1313a27c405_base (\x1 ->
                                                            fmap BG.toFFIType (fun0 (BG.fromFFIType x1))))

-- __unique:__ @instance FromFunPtr (S -> IO BG.CInt)@
foreign import ccall safe "dynamic" hs_bindgen_e86a5fa3f995e6fb_base ::
     BG.FunPtr (BG.CShort -> IO BG.CInt)
  -> BG.CShort -> IO BG.CInt

-- __unique:__ @instance FromFunPtr (S -> IO BG.CInt)@
hs_bindgen_e86a5fa3f995e6fb ::
     BG.FunPtr (S -> IO BG.CInt)
  -> S -> IO BG.CInt
hs_bindgen_e86a5fa3f995e6fb =
  \funPtr0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_e86a5fa3f995e6fb_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))
