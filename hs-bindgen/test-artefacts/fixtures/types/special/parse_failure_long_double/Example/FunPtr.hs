{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fun2
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "/* test_typesspecialparse_failure_lo_Example_get_fun2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d61a16f2d29260ed (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fun2;"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2@
foreign import ccall unsafe "hs_bindgen_d61a16f2d29260ed" hs_bindgen_d61a16f2d29260ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2@
hs_bindgen_d61a16f2d29260ed :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_d61a16f2d29260ed =
  BG.fromFFIType hs_bindgen_d61a16f2d29260ed_base

{-# NOINLINE fun2 #-}
{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 :: BG.FunPtr (BG.CInt -> IO ())
fun2 = BG.unsafePerformIO hs_bindgen_d61a16f2d29260ed
