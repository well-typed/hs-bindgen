{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fun
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/heap_types/struct_const.h>"
  , "/* test_functionsheap_typesstruct_co_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T const (*hs_bindgen_564814e9ac709cad (void)) ("
  , "  T const arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_564814e9ac709cad" hs_bindgen_564814e9ac709cad_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsheap_typesstruct_co_Example_get_fun@
hs_bindgen_564814e9ac709cad :: IO (BG.FunPtr (T -> IO T))
hs_bindgen_564814e9ac709cad =
  BG.fromFFIType hs_bindgen_564814e9ac709cad_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const.h 9:9@

    __exported by:__ @functions\/heap_types\/struct_const.h@
-}
fun :: BG.FunPtr (T -> IO T)
fun = BG.unsafePerformIO hs_bindgen_564814e9ac709cad
