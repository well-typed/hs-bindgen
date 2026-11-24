{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.h
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/squashed_typedef.h>"
  , "void hs_bindgen_cfc42f16d6bff675 ("
  , "  void (*arg1) ("
  , "  T *arg1"
  , ")"
  , ")"
  , "{"
  , "  (h)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancessq_Example_Safe_h@
foreign import ccall safe "hs_bindgen_cfc42f16d6bff675" hs_bindgen_cfc42f16d6bff675_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancessq_Example_Safe_h@
hs_bindgen_cfc42f16d6bff675 ::
     BG.FunPtr (BG.Ptr T -> IO ())
  -> IO ()
hs_bindgen_cfc42f16d6bff675 =
  \x0 ->
    hs_bindgen_cfc42f16d6bff675_base (BG.toFFIType x0)

{-| __C declaration:__ @h@

    __defined at:__ @functions\/fun_ptr_instances\/squashed_typedef.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/squashed_typedef.h@
-}
h ::
     BG.FunPtr (BG.Ptr T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
h = hs_bindgen_cfc42f16d6bff675
