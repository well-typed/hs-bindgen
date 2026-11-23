{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    , Example.FunPtr.n
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/const_void_result.h>"
  , "/* test_functionsfun_ptr_instancesco_Example_get_f */"
  , "__attribute__ ((const))"
  , "void const (*hs_bindgen_6f2316673467ed12 (void)) (void)"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_functionsfun_ptr_instancesco_Example_get_n */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fff74c063dfee06f (void)) ("
  , "  void const (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_get_f@
foreign import ccall unsafe "hs_bindgen_6f2316673467ed12" hs_bindgen_6f2316673467ed12_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_get_f@
hs_bindgen_6f2316673467ed12 :: IO (BG.FunPtr (IO ()))
hs_bindgen_6f2316673467ed12 =
  fmap BG.fromFFIType hs_bindgen_6f2316673467ed12_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 5:12@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
f :: BG.FunPtr (IO ())
f = BG.unsafePerformIO hs_bindgen_6f2316673467ed12

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_get_n@
foreign import ccall unsafe "hs_bindgen_fff74c063dfee06f" hs_bindgen_fff74c063dfee06f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_get_n@
hs_bindgen_fff74c063dfee06f :: IO (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()))
hs_bindgen_fff74c063dfee06f =
  fmap BG.fromFFIType hs_bindgen_fff74c063dfee06f_base

{-# NOINLINE n #-}
{-| __C declaration:__ @n@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
n :: BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
n = BG.unsafePerformIO hs_bindgen_fff74c063dfee06f
