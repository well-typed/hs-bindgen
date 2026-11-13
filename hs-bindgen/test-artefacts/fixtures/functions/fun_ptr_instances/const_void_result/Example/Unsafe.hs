{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    , Example.Unsafe.n
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/const_void_result.h>"
  , "void hs_bindgen_57fb81768400f2fc (void)"
  , "{"
  , "  (f)();"
  , "}"
  , "void hs_bindgen_bbfba49ff2be7d3f ("
  , "  void const (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  (n)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_57fb81768400f2fc" hs_bindgen_57fb81768400f2fc_base ::
     IO ()

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Unsafe_f@
hs_bindgen_57fb81768400f2fc :: IO ()
hs_bindgen_57fb81768400f2fc =
  hs_bindgen_57fb81768400f2fc_base

{-| __C declaration:__ @f@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 5:12@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
f :: IO ()
f = hs_bindgen_57fb81768400f2fc

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Unsafe_n@
foreign import ccall unsafe "hs_bindgen_bbfba49ff2be7d3f" hs_bindgen_bbfba49ff2be7d3f_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Unsafe_n@
hs_bindgen_bbfba49ff2be7d3f ::
     BG.FunPtr (T -> IO ())
  -> IO ()
hs_bindgen_bbfba49ff2be7d3f =
  \x0 ->
    hs_bindgen_bbfba49ff2be7d3f_base (BG.toFFIType x0)

{-| __C declaration:__ @n@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
n ::
     BG.FunPtr (T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
n = hs_bindgen_bbfba49ff2be7d3f
