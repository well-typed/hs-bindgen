{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "void hs_bindgen_a1252a3becef09a6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  fun2(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Safe_fun2@
foreign import ccall safe "hs_bindgen_a1252a3becef09a6" hs_bindgen_a1252a3becef09a6_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Safe_fun2@
hs_bindgen_a1252a3becef09a6 ::
     RIP.CInt
  -> IO ()
hs_bindgen_a1252a3becef09a6 =
  RIP.fromFFIType hs_bindgen_a1252a3becef09a6_base

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 ::
     RIP.CInt
  -> IO ()
fun2 = hs_bindgen_a1252a3becef09a6
