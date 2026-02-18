{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "void hs_bindgen_61793546aa44e36b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  fun2(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Unsafe_fun2@
foreign import ccall unsafe "hs_bindgen_61793546aa44e36b" hs_bindgen_61793546aa44e36b_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Unsafe_fun2@
hs_bindgen_61793546aa44e36b ::
     RIP.CInt
  -> IO ()
hs_bindgen_61793546aa44e36b =
  RIP.fromFFIType hs_bindgen_61793546aa44e36b_base

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 ::
     RIP.CInt
  -> IO ()
fun2 = hs_bindgen_61793546aa44e36b
