{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    , Example.Safe.n
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/const_void_result.h>"
  , "void hs_bindgen_ff5885a24b218ebf (void)"
  , "{"
  , "  (f)();"
  , "}"
  , "void hs_bindgen_a67ac6388237bfd7 ("
  , "  void const (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  (n)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Safe_f@
foreign import ccall safe "hs_bindgen_ff5885a24b218ebf" hs_bindgen_ff5885a24b218ebf_base ::
     IO ()

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Safe_f@
hs_bindgen_ff5885a24b218ebf :: IO ()
hs_bindgen_ff5885a24b218ebf =
  hs_bindgen_ff5885a24b218ebf_base

{-| __C declaration:__ @f@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 5:12@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
f :: IO ()
f = hs_bindgen_ff5885a24b218ebf

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Safe_n@
foreign import ccall safe "hs_bindgen_a67ac6388237bfd7" hs_bindgen_a67ac6388237bfd7_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesco_Example_Safe_n@
hs_bindgen_a67ac6388237bfd7 ::
     BG.FunPtr (T -> IO ())
  -> IO ()
hs_bindgen_a67ac6388237bfd7 =
  \x0 ->
    hs_bindgen_a67ac6388237bfd7_base (BG.toFFIType x0)

{-| __C declaration:__ @n@

    __defined at:__ @functions\/fun_ptr_instances\/const_void_result.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/const_void_result.h@
-}
n ::
     BG.FunPtr (T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
n = hs_bindgen_a67ac6388237bfd7
