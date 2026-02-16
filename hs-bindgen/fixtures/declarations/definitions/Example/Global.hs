{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationsdefinitions_Example_get_n@
hs_bindgen_417f0d4479c97357 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_417f0d4479c97357 =
  RIP.fromFFIType hs_bindgen_417f0d4479c97357_base

{-# NOINLINE n #-}
{-| __C declaration:__ @n@

    __defined at:__ @declarations\/definitions.h 18:5@

    __exported by:__ @declarations\/definitions.h@
-}
n :: RIP.Ptr RIP.CInt
n = RIP.unsafePerformIO hs_bindgen_417f0d4479c97357
