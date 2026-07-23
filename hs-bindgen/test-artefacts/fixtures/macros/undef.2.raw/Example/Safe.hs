{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/undef.h>"
  , "void hs_bindgen_21a957e75025b18b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_2_raw_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_21a957e75025b18b" hs_bindgen_21a957e75025b18b_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_2_raw_Example_Safe_foo@
hs_bindgen_21a957e75025b18b ::
     BG.CInt
  -> IO ()
hs_bindgen_21a957e75025b18b =
  BG.fromFFIType hs_bindgen_21a957e75025b18b_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_21a957e75025b18b
