{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/struct_const_typedef.h>"
  , "void hs_bindgen_4351e21e32969011 ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_4351e21e32969011" hs_bindgen_4351e21e32969011 ::
     HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> IO ()

{-| Pointer-based API for 'fun'
-}
fun_wrapper ::
     HsBindgen.Runtime.ConstPtr.ConstPtr T
     -- ^ __C declaration:__ @x@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> IO ()
fun_wrapper = hs_bindgen_4351e21e32969011

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 9:3@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    F.with x0 (\y1 ->
                 HsBindgen.Runtime.CAPI.allocaAndPeek (\z2 ->
                                                         hs_bindgen_4351e21e32969011 (HsBindgen.Runtime.ConstPtr.ConstPtr y1) z2))
