{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.fun
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/heap_types/struct_const_typedef.h>"
  , "void hs_bindgen_4351e21e32969011 ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = (fun)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_4351e21e32969011" hs_bindgen_4351e21e32969011_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Unsafe_fun@
hs_bindgen_4351e21e32969011 ::
     PtrConst.PtrConst T
  -> PtrConst.PtrConst T
  -> IO ()
hs_bindgen_4351e21e32969011 =
  BG.fromFFIType hs_bindgen_4351e21e32969011_base

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
    BG.with x0 (\x1 ->
                  BG.allocaAndPeek (\res2 ->
                                      hs_bindgen_4351e21e32969011 (PtrConst.unsafeFromPtr x1) res2))
