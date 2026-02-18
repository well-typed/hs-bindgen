{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "/* test_functionsheap_typesstruct_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_071e2eda58051e4a (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_071e2eda58051e4a" hs_bindgen_071e2eda58051e4a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesstruct_Example_get_fun@
hs_bindgen_071e2eda58051e4a :: IO (RIP.FunPtr (T -> IO T))
hs_bindgen_071e2eda58051e4a =
  RIP.fromFFIType hs_bindgen_071e2eda58051e4a_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct.h 9:3@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
fun :: RIP.FunPtr (T -> IO T)
fun = RIP.unsafePerformIO hs_bindgen_071e2eda58051e4a
