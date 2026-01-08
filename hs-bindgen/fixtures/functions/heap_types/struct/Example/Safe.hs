{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "void hs_bindgen_a5d53f538e59b1fc ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_a5d53f538e59b1fc" hs_bindgen_a5d53f538e59b1fc ::
     Ptr.Ptr T
  -> Ptr.Ptr T
  -> IO ()

{-| Pointer-based API for 'fun'
-}
fun_wrapper ::
     Ptr.Ptr T
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr T
  -> IO ()
fun_wrapper = hs_bindgen_a5d53f538e59b1fc

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct.h 9:3@

    __exported by:__ @functions\/heap_types\/struct.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    F.with x0 (\y1 ->
                 HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                                         hs_bindgen_a5d53f538e59b1fc y1 z2))
