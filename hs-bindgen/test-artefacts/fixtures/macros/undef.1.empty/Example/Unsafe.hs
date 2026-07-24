{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/undef.h>"
  , "void hs_bindgen_e7a8db67218e71f8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_1_empty_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_e7a8db67218e71f8" hs_bindgen_e7a8db67218e71f8_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_1_empty_Example_Unsafe_foo@
hs_bindgen_e7a8db67218e71f8 ::
     BG.CInt
  -> IO ()
hs_bindgen_e7a8db67218e71f8 =
  BG.fromFFIType hs_bindgen_e7a8db67218e71f8_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_e7a8db67218e71f8
