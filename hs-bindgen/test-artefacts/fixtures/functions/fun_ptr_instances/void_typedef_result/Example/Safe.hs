{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.g
    , Example.Safe.k
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/void_typedef_result.h>"
  , "void hs_bindgen_740066311785f776 ("
  , "  V (*arg1) (void)"
  , ")"
  , "{"
  , "  (g)(arg1);"
  , "}"
  , "void hs_bindgen_f0890062cbfca39e ("
  , "  V (*arg1) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , ")"
  , "{"
  , "  (k)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_Safe_g@
foreign import ccall safe "hs_bindgen_740066311785f776" hs_bindgen_740066311785f776_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_Safe_g@
hs_bindgen_740066311785f776 ::
     BG.FunPtr (IO V)
  -> IO ()
hs_bindgen_740066311785f776 =
  \x0 ->
    hs_bindgen_740066311785f776_base (BG.toFFIType x0)

{-| __C declaration:__ @g@

    __defined at:__ @functions\/fun_ptr_instances\/void_typedef_result.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/void_typedef_result.h@
-}
g ::
     BG.FunPtr (IO V)
     -- ^ __C declaration:__ @x@
  -> IO ()
g = hs_bindgen_740066311785f776

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_Safe_k@
foreign import ccall safe "hs_bindgen_f0890062cbfca39e" hs_bindgen_f0890062cbfca39e_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesvo_Example_Safe_k@
hs_bindgen_f0890062cbfca39e ::
     BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO V)
  -> IO ()
hs_bindgen_f0890062cbfca39e =
  \x0 ->
    hs_bindgen_f0890062cbfca39e_base (BG.toFFIType x0)

{-| __C declaration:__ @k@

    __defined at:__ @functions\/fun_ptr_instances\/void_typedef_result.h 13:6@

    __exported by:__ @functions\/fun_ptr_instances\/void_typedef_result.h@
-}
k ::
     BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO V)
     -- ^ __C declaration:__ @x@
  -> IO ()
k = hs_bindgen_f0890062cbfca39e
