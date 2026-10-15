{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Example
import Prelude (IO, fmap)

-- __unique:__ @toInt2int@
foreign import ccall safe "wrapper" hs_bindgen_a6c7dd49f5b9d470_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toInt2int@
hs_bindgen_a6c7dd49f5b9d470 ::
     Int2int
  -> IO (BG.FunPtr Int2int)
hs_bindgen_a6c7dd49f5b9d470 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_a6c7dd49f5b9d470_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapInt2int" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromInt2int@
foreign import ccall safe "dynamic" hs_bindgen_65378a8a3cf640ad_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromInt2int@
hs_bindgen_65378a8a3cf640ad ::
     BG.FunPtr Int2int
  -> Int2int
hs_bindgen_65378a8a3cf640ad =
  \funPtr0 ->
    Int2int (\x1 ->
               fmap BG.fromFFIType (hs_bindgen_65378a8a3cf640ad_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))

-- __unique:__ @toFunctionPointer_Function_Aux@
foreign import ccall safe "wrapper" hs_bindgen_b171c028cdc0781d_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toFunctionPointer_Function_Aux@
hs_bindgen_b171c028cdc0781d ::
     FunctionPointer_Function_Aux
  -> IO (BG.FunPtr FunctionPointer_Function_Aux)
hs_bindgen_b171c028cdc0781d =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_b171c028cdc0781d_base (BG.getField @"unwrapFunctionPointer_Function_Aux" fun0))

-- __unique:__ @fromFunctionPointer_Function_Aux@
foreign import ccall safe "dynamic" hs_bindgen_4c3da8240a31e036_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromFunctionPointer_Function_Aux@
hs_bindgen_4c3da8240a31e036 ::
     BG.FunPtr FunctionPointer_Function_Aux
  -> FunctionPointer_Function_Aux
hs_bindgen_4c3da8240a31e036 =
  \funPtr0 ->
    FunctionPointer_Function_Aux (hs_bindgen_4c3da8240a31e036_base (BG.castFunPtr funPtr0))

-- __unique:__ @toNonFunctionPointer_Function@
foreign import ccall safe "wrapper" hs_bindgen_766ae751d60365e9_base ::
     (BG.CInt -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.CInt -> IO BG.CInt))

-- __unique:__ @toNonFunctionPointer_Function@
hs_bindgen_766ae751d60365e9 ::
     NonFunctionPointer_Function
  -> IO (BG.FunPtr NonFunctionPointer_Function)
hs_bindgen_766ae751d60365e9 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_766ae751d60365e9_base (\x1 ->
                                                            fmap BG.toFFIType (BG.getField @"unwrapNonFunctionPointer_Function" fun0 (BG.fromFFIType x1))))

-- __unique:__ @fromNonFunctionPointer_Function@
foreign import ccall safe "dynamic" hs_bindgen_36c7108d046bcbc3_base ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> BG.CInt -> IO BG.CInt

-- __unique:__ @fromNonFunctionPointer_Function@
hs_bindgen_36c7108d046bcbc3 ::
     BG.FunPtr NonFunctionPointer_Function
  -> NonFunctionPointer_Function
hs_bindgen_36c7108d046bcbc3 =
  \funPtr0 ->
    NonFunctionPointer_Function (\x1 ->
                                   fmap BG.fromFFIType (hs_bindgen_36c7108d046bcbc3_base (BG.castFunPtr funPtr0) (BG.toFFIType x1)))

-- __unique:__ @toF1_Aux@
foreign import ccall safe "wrapper" hs_bindgen_00d16e666202ed6c_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toF1_Aux@
hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)
hs_bindgen_00d16e666202ed6c =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_00d16e666202ed6c_base (BG.getField @"unwrapF1_Aux" fun0))

-- __unique:__ @fromF1_Aux@
foreign import ccall safe "dynamic" hs_bindgen_ddeb5206e8192425_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromF1_Aux@
hs_bindgen_ddeb5206e8192425 ::
     BG.FunPtr F1_Aux
  -> F1_Aux
hs_bindgen_ddeb5206e8192425 =
  \funPtr0 ->
    F1_Aux (hs_bindgen_ddeb5206e8192425_base (BG.castFunPtr funPtr0))

-- __unique:__ @toG1@
foreign import ccall safe "wrapper" hs_bindgen_fa5806570b682579_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toG1@
hs_bindgen_fa5806570b682579 ::
     G1
  -> IO (BG.FunPtr G1)
hs_bindgen_fa5806570b682579 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_fa5806570b682579_base (BG.getField @"unwrapG1" fun0))

-- __unique:__ @fromG1@
foreign import ccall safe "dynamic" hs_bindgen_8405c8e75aa78be5_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromG1@
hs_bindgen_8405c8e75aa78be5 ::
     BG.FunPtr G1
  -> G1
hs_bindgen_8405c8e75aa78be5 =
  \funPtr0 ->
    G1 (hs_bindgen_8405c8e75aa78be5_base (BG.castFunPtr funPtr0))

-- __unique:__ @toH1@
foreign import ccall safe "wrapper" hs_bindgen_ffae0d1234ed018f_base ::
     IO ()
  -> IO (BG.FunPtr (IO ()))

-- __unique:__ @toH1@
hs_bindgen_ffae0d1234ed018f ::
     H1
  -> IO (BG.FunPtr H1)
hs_bindgen_ffae0d1234ed018f =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_ffae0d1234ed018f_base (BG.getField @"unwrapH1" fun0))

-- __unique:__ @fromH1@
foreign import ccall safe "dynamic" hs_bindgen_1a33688324e1924f_base ::
     BG.FunPtr (IO ())
  -> IO ()

-- __unique:__ @fromH1@
hs_bindgen_1a33688324e1924f ::
     BG.FunPtr H1
  -> H1
hs_bindgen_1a33688324e1924f =
  \funPtr0 ->
    H1 (hs_bindgen_1a33688324e1924f_base (BG.castFunPtr funPtr0))
