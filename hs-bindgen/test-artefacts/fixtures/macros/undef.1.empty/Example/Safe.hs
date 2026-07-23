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
  , "void hs_bindgen_9537cfc61fc6a78a ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_1_empty_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_9537cfc61fc6a78a" hs_bindgen_9537cfc61fc6a78a_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_1_empty_Example_Safe_foo@
hs_bindgen_9537cfc61fc6a78a ::
     BG.CInt
  -> IO ()
hs_bindgen_9537cfc61fc6a78a =
  BG.fromFFIType hs_bindgen_9537cfc61fc6a78a_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_9537cfc61fc6a78a
