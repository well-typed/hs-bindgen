{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/struct_const.h>"
  , "void hs_bindgen_67465eb5641985dc ("
  , "  T const *arg1,"
  , "  T const *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_67465eb5641985dc" hs_bindgen_67465eb5641985dc ::
     HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> IO ()

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const.h 9:9@

    __exported by:__ @functions\/heap_types\/struct_const.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    F.with x0 (\x1 ->
                 HsBindgen.Runtime.CAPI.allocaAndPeek (\res2 ->
                                                         hs_bindgen_67465eb5641985dc (HsBindgen.Runtime.ConstPtr.ConstPtr x1) res2))
