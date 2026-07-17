{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.n
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "/* test_declarationsdefinitions_Example_get_n */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_417f0d4479c97357 (void)"
  , "{"
  , "  return &n;"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_get_n@
foreign import ccall unsafe "hs_bindgen_417f0d4479c97357" hs_bindgen_417f0d4479c97357_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_declarationsdefinitions_Example_get_n@
hs_bindgen_417f0d4479c97357 :: IO (BG.Ptr BG.CInt)
hs_bindgen_417f0d4479c97357 =
  BG.fromFFIType hs_bindgen_417f0d4479c97357_base

{-# NOINLINE n #-}
{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h 18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n :: BG.Ptr BG.CInt
n = BG.unsafePerformIO hs_bindgen_417f0d4479c97357
