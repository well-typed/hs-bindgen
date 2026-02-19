{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/union_const_typedef.h>"
  , "void hs_bindgen_4e22c71ca196dc5e ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = (fun)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_con_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_4e22c71ca196dc5e" hs_bindgen_4e22c71ca196dc5e_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesunion_con_Example_Unsafe_fun@
hs_bindgen_4e22c71ca196dc5e ::
     PtrConst.PtrConst T
  -> PtrConst.PtrConst T
  -> IO ()
hs_bindgen_4e22c71ca196dc5e =
  RIP.fromFFIType hs_bindgen_4e22c71ca196dc5e_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 9:3@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    RIP.with x0 (\x1 ->
                   RIP.allocaAndPeek (\res2 ->
                                        hs_bindgen_4e22c71ca196dc5e (PtrConst.unsafeFromPtr x1) res2))
