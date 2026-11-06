{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_types.h>"
  , "/* test_macrosmacro_types_2_raw_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a04c0d60680c41e5 (void)) ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_types_2_raw_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_a04c0d60680c41e5" hs_bindgen_a04c0d60680c41e5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_types_2_raw_Example_get_foo@
hs_bindgen_a04c0d60680c41e5 :: IO (BG.FunPtr (BG.CUChar -> IO ()))
hs_bindgen_a04c0d60680c41e5 =
  fmap BG.fromFFIType hs_bindgen_a04c0d60680c41e5_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/macro_types.h 18:6@

    __exported by:__ @macros\/macro_types.h@
-}
foo :: BG.FunPtr (BG.CUChar -> IO ())
foo = BG.unsafePerformIO hs_bindgen_a04c0d60680c41e5
