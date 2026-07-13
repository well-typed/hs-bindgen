{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosundef_Example_get_foo@
hs_bindgen_71864e8181a7e9dd :: IO (BG.FunPtr (T -> IO ()))
hs_bindgen_71864e8181a7e9dd =
  BG.fromFFIType hs_bindgen_71864e8181a7e9dd_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo :: BG.FunPtr (T -> IO ())
foo = BG.unsafePerformIO hs_bindgen_71864e8181a7e9dd
