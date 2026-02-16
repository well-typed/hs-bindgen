{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/union.h>"
  , "void hs_bindgen_9fc5746860ab93cb ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_9fc5746860ab93cb" hs_bindgen_9fc5746860ab93cb_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesunion_Example_Safe_fun@
hs_bindgen_9fc5746860ab93cb ::
     RIP.Ptr T
  -> RIP.Ptr T
  -> IO ()
hs_bindgen_9fc5746860ab93cb =
  RIP.fromFFIType hs_bindgen_9fc5746860ab93cb_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union.h 9:3@

    __exported by:__ @functions\/heap_types\/union.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    RIP.with x0 (\x1 ->
                   RIP.allocaAndPeek (\res2 ->
                                        hs_bindgen_9fc5746860ab93cb x1 res2))
