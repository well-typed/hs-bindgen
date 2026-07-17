{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.global
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <comprehensive/smoke.h>"
  , "/* test_comprehensivesmoke_Example_get_global */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_dbeef9adb3c4fba3 (void)"
  , "{"
  , "  return &global;"
  , "}"
  ]))

-- __unique:__ @test_comprehensivesmoke_Example_get_global@
foreign import ccall unsafe "hs_bindgen_dbeef9adb3c4fba3" hs_bindgen_dbeef9adb3c4fba3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_global@
hs_bindgen_dbeef9adb3c4fba3 :: IO (BG.Ptr BG.CInt)
hs_bindgen_dbeef9adb3c4fba3 =
  BG.fromFFIType hs_bindgen_dbeef9adb3c4fba3_base

{-# NOINLINE global #-}
{-| __C declaration:__ @global@

    __defined at:__ @comprehensive\/smoke.h 78:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
global :: BG.Ptr BG.CInt
global =
  BG.unsafePerformIO hs_bindgen_dbeef9adb3c4fba3
