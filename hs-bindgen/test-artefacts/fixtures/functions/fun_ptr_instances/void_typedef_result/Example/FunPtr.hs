{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.g
    , Example.FunPtr.k
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/void_typedef_result.h>"
  , "/* test_functionsfun_ptr_instancesvo_Example_get_g */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_855882c031315f1f (void)) ("
  , "  V (*arg1) (void)"
  , ")"
  , "{"
  , "  return &g;"
  , "}"
  , "/* test_functionsfun_ptr_instancesvo_Example_get_k */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3d59825292489f4b (void)) ("
  , "  V (*arg1) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , ")"
  , "{"
  , "  return &k;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_get_g@
foreign import ccall unsafe "hs_bindgen_855882c031315f1f" hs_bindgen_855882c031315f1f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_get_g@
hs_bindgen_855882c031315f1f :: IO (BG.FunPtr (BG.FunPtr (IO V) -> IO ()))
hs_bindgen_855882c031315f1f =
  fmap BG.fromFFIType hs_bindgen_855882c031315f1f_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @functions\/fun_ptr_instances\/void_typedef_result.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/void_typedef_result.h@
-}
g :: BG.FunPtr (BG.FunPtr (IO V) -> IO ())
g = BG.unsafePerformIO hs_bindgen_855882c031315f1f

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_get_k@
foreign import ccall unsafe "hs_bindgen_3d59825292489f4b" hs_bindgen_3d59825292489f4b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_get_k@
hs_bindgen_3d59825292489f4b :: IO (BG.FunPtr (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO V) -> IO ()))
hs_bindgen_3d59825292489f4b =
  fmap BG.fromFFIType hs_bindgen_3d59825292489f4b_base

{-# NOINLINE k #-}
{-| __C declaration:__ @k@

    __defined at:__ @functions\/fun_ptr_instances\/void_typedef_result.h 13:6@

    __exported by:__ @functions\/fun_ptr_instances\/void_typedef_result.h@
-}
k :: BG.FunPtr (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO V) -> IO ())
k = BG.unsafePerformIO hs_bindgen_3d59825292489f4b
