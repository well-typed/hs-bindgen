{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/struct.h>"
  , "void hs_bindgen_c4af6bb824712c6a ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_c4af6bb824712c6a" hs_bindgen_c4af6bb824712c6a ::
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
fun_wrapper = hs_bindgen_c4af6bb824712c6a

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
                                                         hs_bindgen_c4af6bb824712c6a y1 z2))
