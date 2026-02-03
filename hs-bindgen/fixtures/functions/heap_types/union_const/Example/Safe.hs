{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/union_const.h>"
  , "void hs_bindgen_8a303cd5b4f7787b ("
  , "  T const *arg1,"
  , "  T const *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_con_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_8a303cd5b4f7787b" hs_bindgen_8a303cd5b4f7787b_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesunion_con_Example_Safe_fun@
hs_bindgen_8a303cd5b4f7787b ::
     HsBindgen.Runtime.PtrConst.PtrConst T
  -> HsBindgen.Runtime.PtrConst.PtrConst T
  -> IO ()
hs_bindgen_8a303cd5b4f7787b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_8a303cd5b4f7787b_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const.h 9:9@

    __exported by:__ @functions\/heap_types\/union_const.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    F.with x0 (\x1 ->
                 HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res2 ->
                                                                  hs_bindgen_8a303cd5b4f7787b (HsBindgen.Runtime.PtrConst.unsafeFromPtr x1) res2))
