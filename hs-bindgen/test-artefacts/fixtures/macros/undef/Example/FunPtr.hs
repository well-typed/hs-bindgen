{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/undef.h>"
  , "/* test_macrosundef_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_71864e8181a7e9dd (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_71864e8181a7e9dd" hs_bindgen_71864e8181a7e9dd_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosundef_Example_get_foo@
hs_bindgen_71864e8181a7e9dd :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_71864e8181a7e9dd =
  RIP.fromFFIType hs_bindgen_71864e8181a7e9dd_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo :: RIP.FunPtr (T -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_71864e8181a7e9dd
