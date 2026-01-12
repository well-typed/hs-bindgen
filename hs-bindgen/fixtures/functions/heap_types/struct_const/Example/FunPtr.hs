{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
foreign import ccall unsafe "hs_bindgen_564814e9ac709cad" hs_bindgen_564814e9ac709cad ::
     IO (Ptr.FunPtr (T -> IO T))

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const.h 9:9@

    __exported by:__ @functions\/heap_types\/struct_const.h@
-}
fun :: Ptr.FunPtr (T -> IO T)
fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_564814e9ac709cad
