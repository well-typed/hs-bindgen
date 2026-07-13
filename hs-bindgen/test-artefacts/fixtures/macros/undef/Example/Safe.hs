{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/undef.h>"
  , "void hs_bindgen_2a245181e5fb1e1c ("
  , "  T arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosundef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_2a245181e5fb1e1c" hs_bindgen_2a245181e5fb1e1c_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_Example_Safe_foo@
hs_bindgen_2a245181e5fb1e1c ::
     T
  -> IO ()
hs_bindgen_2a245181e5fb1e1c =
  BG.fromFFIType hs_bindgen_2a245181e5fb1e1c_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     T
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_2a245181e5fb1e1c
