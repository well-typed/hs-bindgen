{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toF1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     (BG.CInt -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_00d16e666202ed6c_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapF1_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromF1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
  -> BG.CInt -> BG.CInt -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     BG.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    F1_Aux (\x1 ->
              \x2 ->
                hs_bindgen_ddeb5206e8192425_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

-- __unique:__ @toF2_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c39d7524b75b54e8_base ::
     (BG.CInt -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))

-- __unique:__ @toF2_Aux@
hs_bindgen_c39d7524b75b54e8 ::
     F2_Aux
  -> IO (BG.FunPtr F2_Aux)
hs_bindgen_c39d7524b75b54e8 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c39d7524b75b54e8_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapF2_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromF2_Aux@
foreign import ccall safe "dynamic" hs_bindgen_e15bcd26f1ed1df7_base ::
     BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
  -> BG.CInt -> BG.CInt -> IO ()

-- __unique:__ @fromF2_Aux@
hs_bindgen_e15bcd26f1ed1df7 ::
     BG.FunPtr F2_Aux
  -> F2_Aux
hs_bindgen_e15bcd26f1ed1df7 =
  \funPtr0 ->
    F2_Aux (\x1 ->
              \x2 ->
                hs_bindgen_e15bcd26f1ed1df7_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

-- __unique:__ @toF3_Aux@
foreign import ccall safe "wrapper" hs_bindgen_4a960721e7d1dcef_base ::
     (BG.CInt -> BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> BG.CInt -> IO ()))

-- __unique:__ @toF3_Aux@
hs_bindgen_4a960721e7d1dcef ::
     F3_Aux
  -> IO (BG.FunPtr F3_Aux)
hs_bindgen_4a960721e7d1dcef =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_4a960721e7d1dcef_base (\x1 ->
                                                            \x2 ->
                                                              BG.getField @"unwrapF3_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @fromF3_Aux@
foreign import ccall safe "dynamic" hs_bindgen_66460422a7197535_base ::
     BG.FunPtr (BG.CInt -> BG.CInt -> IO ())
  -> BG.CInt -> BG.CInt -> IO ()

-- __unique:__ @fromF3_Aux@
hs_bindgen_66460422a7197535 ::
     BG.FunPtr F3_Aux
  -> F3_Aux
hs_bindgen_66460422a7197535 =
  \funPtr0 ->
    F3_Aux (\x1 ->
              \x2 ->
                hs_bindgen_66460422a7197535_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2))

-- __unique:__ @toF4_Aux@
foreign import ccall safe "wrapper" hs_bindgen_83bcff023b3bc648_base ::
     IO BG.CInt
  -> IO (BG.FunPtr (IO BG.CInt))

-- __unique:__ @toF4_Aux@
hs_bindgen_83bcff023b3bc648 ::
     F4_Aux
  -> IO (BG.FunPtr F4_Aux)
hs_bindgen_83bcff023b3bc648 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_83bcff023b3bc648_base (fmap BG.toFFIType (BG.getField @"unwrapF4_Aux" fun0)))

-- __unique:__ @fromF4_Aux@
foreign import ccall safe "dynamic" hs_bindgen_40f9a8d432b9eb97_base ::
     BG.FunPtr (IO BG.CInt)
  -> IO BG.CInt

-- __unique:__ @fromF4_Aux@
hs_bindgen_40f9a8d432b9eb97 ::
     BG.FunPtr F4_Aux
  -> F4_Aux
hs_bindgen_40f9a8d432b9eb97 =
  \funPtr0 ->
    F4_Aux (fmap BG.fromFFIType (hs_bindgen_40f9a8d432b9eb97_base (BG.castFunPtr funPtr0)))

-- __unique:__ @toF5_Aux@
foreign import ccall safe "wrapper" hs_bindgen_6891cbd81d6f42b9_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toF5_Aux@
hs_bindgen_6891cbd81d6f42b9 ::
     F5_Aux
  -> IO (BG.FunPtr F5_Aux)
hs_bindgen_6891cbd81d6f42b9 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_6891cbd81d6f42b9_base (BG.getField @"unwrapF5_Aux" fun0))

-- __unique:__ @fromF5_Aux@
foreign import ccall safe "dynamic" hs_bindgen_586f6635c057975f_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF5_Aux@
hs_bindgen_586f6635c057975f ::
     BG.FunPtr F5_Aux
  -> F5_Aux
hs_bindgen_586f6635c057975f =
  \funPtr0 ->
    F5_Aux (hs_bindgen_586f6635c057975f_base (BG.castFunPtr funPtr0))

-- __unique:__ @toF6_Aux@
foreign import ccall safe "wrapper" hs_bindgen_c1baf73f98614f45_base ::
     (BG.CInt -> IO ())
  -> IO (BG.FunPtr (BG.CInt -> IO ()))

-- __unique:__ @toF6_Aux@
hs_bindgen_c1baf73f98614f45 ::
     F6_Aux
  -> IO (BG.FunPtr F6_Aux)
hs_bindgen_c1baf73f98614f45 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c1baf73f98614f45_base (\x1 ->
                                                            BG.getField @"unwrapF6_Aux" fun0 (BG.fromFFIType x1)))

-- __unique:__ @fromF6_Aux@
foreign import ccall safe "dynamic" hs_bindgen_a887947b26e58f0c_base ::
     BG.FunPtr (BG.CInt -> IO ())
  -> BG.CInt -> IO ()

-- __unique:__ @fromF6_Aux@
hs_bindgen_a887947b26e58f0c ::
     BG.FunPtr F6_Aux
  -> F6_Aux
hs_bindgen_a887947b26e58f0c =
  \funPtr0 ->
    F6_Aux (\x1 ->
              hs_bindgen_a887947b26e58f0c_base (BG.castFunPtr funPtr0) (BG.toFFIType x1))
