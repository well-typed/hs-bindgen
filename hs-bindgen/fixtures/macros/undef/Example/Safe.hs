{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosundef_Example_Safe_foo@
hs_bindgen_2a245181e5fb1e1c ::
     T
  -> IO ()
hs_bindgen_2a245181e5fb1e1c =
  RIP.fromFFIType hs_bindgen_2a245181e5fb1e1c_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/undef.h 4:6@

    __exported by:__ @macros\/undef.h@
-}
foo ::
     T
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_2a245181e5fb1e1c
