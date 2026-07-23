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
  , "void hs_bindgen_03f5b7598cff52b9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_2_raw_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_03f5b7598cff52b9" hs_bindgen_03f5b7598cff52b9_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_2_raw_Example_Unsafe_foo@
hs_bindgen_03f5b7598cff52b9 ::
     BG.CInt
  -> IO ()
hs_bindgen_03f5b7598cff52b9 =
  BG.fromFFIType hs_bindgen_03f5b7598cff52b9_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_03f5b7598cff52b9
