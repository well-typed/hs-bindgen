{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_types.h>"
  , "void hs_bindgen_8c082f80a2cd4da1 ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_types_2_raw_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_8c082f80a2cd4da1" hs_bindgen_8c082f80a2cd4da1_base ::
     BG.CUChar
  -> IO ()

-- __unique:__ @test_macrosmacro_types_2_raw_Example_Unsafe_foo@
hs_bindgen_8c082f80a2cd4da1 ::
     BG.CUChar
  -> IO ()
hs_bindgen_8c082f80a2cd4da1 =
  \x0 ->
    hs_bindgen_8c082f80a2cd4da1_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/macro_types.h 18:6@

    __exported by:__ @macros\/macro_types.h@
-}
foo ::
     BG.CUChar
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_8c082f80a2cd4da1
