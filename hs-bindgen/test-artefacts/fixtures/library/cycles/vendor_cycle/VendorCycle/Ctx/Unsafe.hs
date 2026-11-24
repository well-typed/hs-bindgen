{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorCycle.Ctx.Unsafe
    ( VendorCycle.Ctx.Unsafe.ctx_init
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified VendorCycle.Vendor
import Prelude (IO)
import VendorCycle.Ctx

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_cycle.h>"
  , "void hs_bindgen_caf0cfd855ce18c4 ("
  , "  struct lib_ctx *arg1,"
  , "  struct host_api *arg2"
  , ")"
  , "{"
  , "  (ctx_init)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarycyclesvendor_cycle_VendorCycle.Ctx_Unsafe_ctx_init@
foreign import ccall unsafe "hs_bindgen_caf0cfd855ce18c4" hs_bindgen_caf0cfd855ce18c4_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarycyclesvendor_cycle_VendorCycle.Ctx_Unsafe_ctx_init@
hs_bindgen_caf0cfd855ce18c4 ::
     BG.Ptr Lib_ctx
  -> BG.Ptr VendorCycle.Vendor.Host_api
  -> IO ()
hs_bindgen_caf0cfd855ce18c4 =
  \x0 ->
    \x1 ->
      hs_bindgen_caf0cfd855ce18c4_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @ctx_init@

    __defined at:__ @vendor_cycle\/ctx.h 15:6@

    __exported by:__ @vendor_cycle.h@
-}
ctx_init ::
     BG.Ptr Lib_ctx
     -- ^ __C declaration:__ @ctx@
  -> BG.Ptr VendorCycle.Vendor.Host_api
     -- ^ __C declaration:__ @host@
  -> IO ()
ctx_init = hs_bindgen_caf0cfd855ce18c4
