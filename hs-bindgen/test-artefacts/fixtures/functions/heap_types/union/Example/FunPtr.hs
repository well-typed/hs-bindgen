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
  [ "#include <functions/heap_types/union.h>"
  , "/* test_functionsheap_typesunion_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_d5d6e9f882bc09ed (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_d5d6e9f882bc09ed" hs_bindgen_d5d6e9f882bc09ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsheap_typesunion_Example_get_fun@
hs_bindgen_d5d6e9f882bc09ed :: IO (BG.FunPtr (T -> IO T))
hs_bindgen_d5d6e9f882bc09ed =
  BG.fromFFIType hs_bindgen_d5d6e9f882bc09ed_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union.h 9:3@

    __exported by:__ @functions\/heap_types\/union.h@
-}
fun :: BG.FunPtr (T -> IO T)
fun = BG.unsafePerformIO hs_bindgen_d5d6e9f882bc09ed
