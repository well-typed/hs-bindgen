{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.fun2
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "void hs_bindgen_a1252a3becef09a6 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (fun2)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Safe_fun2@
foreign import ccall safe "hs_bindgen_a1252a3becef09a6" hs_bindgen_a1252a3becef09a6_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Safe_fun2@
hs_bindgen_a1252a3becef09a6 ::
     BG.CInt
  -> IO ()
hs_bindgen_a1252a3becef09a6 =
  BG.fromFFIType hs_bindgen_a1252a3becef09a6_base

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 ::
     BG.CInt
  -> IO ()
fun2 = hs_bindgen_a1252a3becef09a6
