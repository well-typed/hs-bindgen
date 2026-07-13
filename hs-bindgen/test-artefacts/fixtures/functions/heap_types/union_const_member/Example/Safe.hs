{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.fun
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/heap_types/union_const_member.h>"
  , "void hs_bindgen_8a303cd5b4f7787b ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = (fun)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_con_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_8a303cd5b4f7787b" hs_bindgen_8a303cd5b4f7787b_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesunion_con_Example_Safe_fun@
hs_bindgen_8a303cd5b4f7787b ::
     BG.Ptr T
  -> BG.Ptr T
  -> IO ()
hs_bindgen_8a303cd5b4f7787b =
  BG.fromFFIType hs_bindgen_8a303cd5b4f7787b_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const_member.h 9:3@

    __exported by:__ @functions\/heap_types\/union_const_member.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    BG.with x0 (\x1 ->
                  BG.allocaAndPeek (\res2 ->
                                      hs_bindgen_8a303cd5b4f7787b x1 res2))
