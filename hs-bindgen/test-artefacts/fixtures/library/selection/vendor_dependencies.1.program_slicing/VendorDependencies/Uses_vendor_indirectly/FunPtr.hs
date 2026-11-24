{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Uses_vendor_indirectly.FunPtr
    ( VendorDependencies.Uses_vendor_indirectly.FunPtr.uses_vendor_indirectly_apply
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.Uses_vendor_indirectly

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "/* test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_indirectly_get_uses_vendor_indirectly_apply */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_fcde7e9818ef64ed (void)) ("
  , "  struct uses_vendor_indirectly *arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &uses_vendor_indirectly_apply;"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_indirectly_get_uses_vendor_indirectly_apply@
foreign import ccall unsafe "hs_bindgen_fcde7e9818ef64ed" hs_bindgen_fcde7e9818ef64ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_indirectly_get_uses_vendor_indirectly_apply@
hs_bindgen_fcde7e9818ef64ed :: IO (BG.FunPtr (BG.Ptr Uses_vendor_indirectly -> BG.CDouble -> IO BG.CDouble))
hs_bindgen_fcde7e9818ef64ed =
  fmap BG.fromFFIType hs_bindgen_fcde7e9818ef64ed_base

{-# NOINLINE uses_vendor_indirectly_apply #-}
{-| __C declaration:__ @uses_vendor_indirectly_apply@

    __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 12:8@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_indirectly_apply :: BG.FunPtr (BG.Ptr Uses_vendor_indirectly -> BG.CDouble -> IO BG.CDouble)
uses_vendor_indirectly_apply =
  BG.unsafePerformIO hs_bindgen_fcde7e9818ef64ed
