{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/struct_const_typedef.h>"
  , "/* test_functionsheap_typesstruct_co_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_564814e9ac709cad (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesstruct_co_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_564814e9ac709cad" hs_bindgen_564814e9ac709cad_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsheap_typesstruct_co_Example_get_fun@
hs_bindgen_564814e9ac709cad :: IO (Ptr.FunPtr (T -> IO T))
hs_bindgen_564814e9ac709cad =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_564814e9ac709cad_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/struct_const_typedef.h 9:3@

    __exported by:__ @functions\/heap_types\/struct_const_typedef.h@
-}
fun :: Ptr.FunPtr (T -> IO T)
fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_564814e9ac709cad
