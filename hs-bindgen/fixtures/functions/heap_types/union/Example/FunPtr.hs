{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesunion_Example_get_fun@
hs_bindgen_d5d6e9f882bc09ed :: IO (RIP.FunPtr (T -> IO T))
hs_bindgen_d5d6e9f882bc09ed =
  RIP.fromFFIType hs_bindgen_d5d6e9f882bc09ed_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union.h 9:3@

    __exported by:__ @functions\/heap_types\/union.h@
-}
fun :: RIP.FunPtr (T -> IO T)
fun = RIP.unsafePerformIO hs_bindgen_d5d6e9f882bc09ed
