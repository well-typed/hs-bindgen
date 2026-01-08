{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/heap_types/union_const_typedef.h>"
  , "/* test_functionsheap_typesunion_con_Example_get_fun */"
  , "__attribute__ ((const))"
  , "T (*hs_bindgen_7e6af500caa71b85 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionsheap_typesunion_con_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_7e6af500caa71b85" hs_bindgen_7e6af500caa71b85 ::
     IO (Ptr.FunPtr (T -> IO T))

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const_typedef.h 9:3@

    __exported by:__ @functions\/heap_types\/union_const_typedef.h@
-}
fun :: Ptr.FunPtr (T -> IO T)
fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e6af500caa71b85
