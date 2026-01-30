{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/struct_const_member.h>"
  , "void hs_bindgen_67465eb5641985dc ("
  , "  T *arg1,"
  , "  T *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Safe_fun@
foreign import ccall safe "hs_bindgen_67465eb5641985dc" hs_bindgen_67465eb5641985dc_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesstruct_co_Example_Safe_fun@
hs_bindgen_67465eb5641985dc ::
     Ptr.Ptr T
  -> Ptr.Ptr T
  -> IO ()
hs_bindgen_67465eb5641985dc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_67465eb5641985dc_base

{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const_member.h 9:3@

    __exported by:__ @functions\/heap_types\/struct_const_member.h@
-}
fun ::
     T
     -- ^ __C declaration:__ @x@
  -> IO T
fun =
  \x0 ->
    F.with x0 (\x1 ->
                 HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res2 ->
                                                                  hs_bindgen_67465eb5641985dc x1 res2))
