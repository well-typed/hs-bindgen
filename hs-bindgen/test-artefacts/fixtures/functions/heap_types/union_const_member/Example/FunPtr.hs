{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fun
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsheap_typesunion_con_Example_get_fun@
hs_bindgen_7e6af500caa71b85 :: IO (BG.FunPtr (T -> IO T))
hs_bindgen_7e6af500caa71b85 =
  BG.fromFFIType hs_bindgen_7e6af500caa71b85_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/heap_types\/union_const_member.h 9:3@

    __exported by:__ @functions\/heap_types\/union_const_member.h@
-}
fun :: BG.FunPtr (T -> IO T)
fun = BG.unsafePerformIO hs_bindgen_7e6af500caa71b85
