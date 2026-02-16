{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2@
hs_bindgen_d61a16f2d29260ed :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_d61a16f2d29260ed =
  RIP.fromFFIType hs_bindgen_d61a16f2d29260ed_base

{-# NOINLINE fun2 #-}
{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 :: RIP.FunPtr (RIP.CInt -> IO ())
fun2 =
  RIP.unsafePerformIO hs_bindgen_d61a16f2d29260ed
