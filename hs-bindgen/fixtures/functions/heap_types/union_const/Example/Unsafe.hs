{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/union_const.h>"
  , "void hs_bindgen_4e22c71ca196dc5e ("
  , "  T const *arg1,"
  , "  T const *arg2"
  , ")"
  , "{"
  , "  *arg2 = fun(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_con_Example_Unsafe_fun@
foreign import ccall unsafe "hs_bindgen_4e22c71ca196dc5e" hs_bindgen_4e22c71ca196dc5e_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_functionsheap_typesunion_con_Example_Unsafe_fun@
hs_bindgen_4e22c71ca196dc5e ::
     HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> HsBindgen.Runtime.ConstPtr.ConstPtr T
  -> IO ()
hs_bindgen_4e22c71ca196dc5e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4e22c71ca196dc5e_base

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
                 HsBindgen.Runtime.CAPI.allocaAndPeek (\res2 ->
                                                         hs_bindgen_4e22c71ca196dc5e (HsBindgen.Runtime.ConstPtr.ConstPtr x1) res2))
