{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.global
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivesmoke_Example_get_global@
hs_bindgen_dbeef9adb3c4fba3 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_dbeef9adb3c4fba3 =
  RIP.fromFFIType hs_bindgen_dbeef9adb3c4fba3_base

{-# NOINLINE global #-}
{-| __C declaration:__ @global@

    __defined at:__ @comprehensive\/smoke.h 78:12@

    __exported by:__ @comprehensive\/smoke.h@
-}
global :: RIP.Ptr RIP.CInt
global =
  RIP.unsafePerformIO hs_bindgen_dbeef9adb3c4fba3
