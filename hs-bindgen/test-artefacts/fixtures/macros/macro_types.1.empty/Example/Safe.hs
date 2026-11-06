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
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_types.h>"
  , "void hs_bindgen_3b0cacc35c4cfb72 ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_types_1_empty_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3b0cacc35c4cfb72" hs_bindgen_3b0cacc35c4cfb72_base ::
     BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosmacro_types_1_empty_Example_Safe_foo@
hs_bindgen_3b0cacc35c4cfb72 ::
     BG.CUChar
  -> IO ()
hs_bindgen_3b0cacc35c4cfb72 =
  \x0 ->
    hs_bindgen_3b0cacc35c4cfb72_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/macro_types.h 18:6@

    __exported by:__ @macros\/macro_types.h@
-}
foo ::
     BG.CUChar
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3b0cacc35c4cfb72
