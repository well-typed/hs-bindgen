{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.length
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/prelude_names.h>"
  , "/* test_edgecasesprelude_names_Example_get_length */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_d73ae441829395d0 (void)"
  , "{"
  , "  return &length;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesprelude_names_Example_get_length@
foreign import ccall unsafe "hs_bindgen_d73ae441829395d0" hs_bindgen_d73ae441829395d0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesprelude_names_Example_get_length@
hs_bindgen_d73ae441829395d0 :: IO (BG.Ptr BG.CInt)
hs_bindgen_d73ae441829395d0 =
  fmap BG.fromFFIType hs_bindgen_d73ae441829395d0_base

{-# NOINLINE length #-}
{-| __C declaration:__ @length@

    __defined at:__ @edge-cases\/prelude_names.h 19:12@

    __exported by:__ @edge-cases\/prelude_names.h@
-}
length :: BG.Ptr BG.CInt
length =
  BG.unsafePerformIO hs_bindgen_d73ae441829395d0
