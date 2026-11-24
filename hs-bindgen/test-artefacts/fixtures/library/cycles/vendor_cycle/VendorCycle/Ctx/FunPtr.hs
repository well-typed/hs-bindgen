{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorCycle.Ctx.FunPtr
    ( VendorCycle.Ctx.FunPtr.ctx_init
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified VendorCycle.Vendor
import Prelude (IO, fmap)
import VendorCycle.Ctx

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_cycle.h>"
  , "/* test_librarycyclesvendor_cycle_VendorCycle.Ctx_get_ctx_init */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d363fc4d2107878a (void)) ("
  , "  struct lib_ctx *arg1,"
  , "  struct host_api *arg2"
  , ")"
  , "{"
  , "  return &ctx_init;"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesvendor_cycle_VendorCycle.Ctx_get_ctx_init@
foreign import ccall unsafe "hs_bindgen_d363fc4d2107878a" hs_bindgen_d363fc4d2107878a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycyclesvendor_cycle_VendorCycle.Ctx_get_ctx_init@
hs_bindgen_d363fc4d2107878a :: IO (BG.FunPtr (BG.Ptr Lib_ctx -> BG.Ptr VendorCycle.Vendor.Host_api -> IO ()))
hs_bindgen_d363fc4d2107878a =
  fmap BG.fromFFIType hs_bindgen_d363fc4d2107878a_base

{-# NOINLINE ctx_init #-}
{-| __C declaration:__ @ctx_init@

    __defined at:__ @vendor_cycle\/ctx.h 15:6@

    __exported by:__ @vendor_cycle.h@
-}
ctx_init :: BG.FunPtr (BG.Ptr Lib_ctx -> BG.Ptr VendorCycle.Vendor.Host_api -> IO ())
ctx_init =
  BG.unsafePerformIO hs_bindgen_d363fc4d2107878a
