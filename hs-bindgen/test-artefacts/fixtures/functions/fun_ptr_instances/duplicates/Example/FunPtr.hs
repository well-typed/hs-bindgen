{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.d
    , Example.FunPtr.e
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/duplicates.h>"
  , "/* test_functionsfun_ptr_instancesdu_Example_get_d */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d46a8eccbac18ea2 (void)) ("
  , "  void (*arg1) ("
  , "  T const arg1"
  , "),"
  , "  void (*arg2) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  return &d;"
  , "}"
  , "/* test_functionsfun_ptr_instancesdu_Example_get_e */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_128acdbed0ef8810 (void)) ("
  , "  void (*arg1) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , "),"
  , "  void (*arg2) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , ")"
  , "{"
  , "  return &e;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_get_d@
foreign import ccall unsafe "hs_bindgen_d46a8eccbac18ea2" hs_bindgen_d46a8eccbac18ea2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_get_d@
hs_bindgen_d46a8eccbac18ea2 :: IO (BG.FunPtr (BG.FunPtr (T -> IO ()) -> BG.FunPtr (T -> IO ()) -> IO ()))
hs_bindgen_d46a8eccbac18ea2 =
  fmap BG.fromFFIType hs_bindgen_d46a8eccbac18ea2_base

{-# NOINLINE d #-}
{-| __C declaration:__ @d@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 6:6@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
d :: BG.FunPtr (BG.FunPtr (T -> IO ()) -> BG.FunPtr (T -> IO ()) -> IO ())
d = BG.unsafePerformIO hs_bindgen_d46a8eccbac18ea2

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_get_e@
foreign import ccall unsafe "hs_bindgen_128acdbed0ef8810" hs_bindgen_128acdbed0ef8810_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_get_e@
hs_bindgen_128acdbed0ef8810 :: IO (BG.FunPtr (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()) -> BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()) -> IO ()))
hs_bindgen_128acdbed0ef8810 =
  fmap BG.fromFFIType hs_bindgen_128acdbed0ef8810_base

{-# NOINLINE e #-}
{-| __C declaration:__ @e@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
e :: BG.FunPtr (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()) -> BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()) -> IO ())
e = BG.unsafePerformIO hs_bindgen_128acdbed0ef8810
