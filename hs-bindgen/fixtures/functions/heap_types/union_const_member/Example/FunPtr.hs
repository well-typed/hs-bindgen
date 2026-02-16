{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/heap_types/union_const_member.h>"
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
foreign import ccall unsafe "hs_bindgen_7e6af500caa71b85" hs_bindgen_7e6af500caa71b85_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsheap_typesunion_con_Example_get_fun@
hs_bindgen_7e6af500caa71b85 :: IO (RIP.FunPtr (T -> IO T))
hs_bindgen_7e6af500caa71b85 =
  RIP.fromFFIType hs_bindgen_7e6af500caa71b85_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const_member.h 9:3@

    __exported by:__ @functions\/heap_types\/union_const_member.h@
-}
fun :: RIP.FunPtr (T -> IO T)
fun = RIP.unsafePerformIO hs_bindgen_7e6af500caa71b85
