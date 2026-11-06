{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_types.h>"
  , "void hs_bindgen_d6a1b98f4ee28d61 ("
  , "  BOOLEAN_T arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_types_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_d6a1b98f4ee28d61" hs_bindgen_d6a1b98f4ee28d61_base ::
     BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosmacro_types_Example_Safe_foo@
hs_bindgen_d6a1b98f4ee28d61 ::
     BOOLEAN_T
  -> IO ()
hs_bindgen_d6a1b98f4ee28d61 =
  \x0 ->
    hs_bindgen_d6a1b98f4ee28d61_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/macro_types.h 18:6@

    __exported by:__ @macros\/macro_types.h@
-}
foo ::
     BOOLEAN_T
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_d6a1b98f4ee28d61
