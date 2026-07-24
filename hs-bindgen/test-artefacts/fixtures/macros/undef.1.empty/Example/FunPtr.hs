{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/undef.h>"
  , "/* test_macrosundef_1_empty_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_517f6ccac933b5bc (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_1_empty_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_517f6ccac933b5bc" hs_bindgen_517f6ccac933b5bc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosundef_1_empty_Example_get_foo@
hs_bindgen_517f6ccac933b5bc :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_517f6ccac933b5bc =
  BG.fromFFIType hs_bindgen_517f6ccac933b5bc_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_517f6ccac933b5bc
