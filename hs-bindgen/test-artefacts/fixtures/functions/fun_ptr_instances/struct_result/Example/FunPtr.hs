{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.h
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/struct_result.h>"
  , "/* test_functionsfun_ptr_instancesst_Example_get_h */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a7286809c160d51a (void)) ("
  , "  T (*arg1) (void)"
  , ")"
  , "{"
  , "  return &h;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesst_Example_get_h@
foreign import ccall unsafe "hs_bindgen_a7286809c160d51a" hs_bindgen_a7286809c160d51a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesst_Example_get_h@
hs_bindgen_a7286809c160d51a :: IO (BG.FunPtr (BG.FunPtr (IO T) -> IO ()))
hs_bindgen_a7286809c160d51a =
  fmap BG.fromFFIType hs_bindgen_a7286809c160d51a_base

{-# NOINLINE h #-}
{-| __C declaration:__ @h@

    __defined at:__ @functions\/fun_ptr_instances\/struct_result.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/struct_result.h@
-}
h :: BG.FunPtr (BG.FunPtr (IO T) -> IO ())
h = BG.unsafePerformIO hs_bindgen_a7286809c160d51a
