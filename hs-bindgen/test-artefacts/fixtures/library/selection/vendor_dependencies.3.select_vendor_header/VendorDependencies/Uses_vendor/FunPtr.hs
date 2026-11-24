{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Uses_vendor.FunPtr
    ( VendorDependencies.Uses_vendor.FunPtr.uses_vendor_take
    , VendorDependencies.Uses_vendor.FunPtr.uses_vendor_age
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified VendorDependencies.No_vendor
import qualified VendorDependencies.Vendor
import Prelude (IO, fmap)
import VendorDependencies.Uses_vendor

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "/* test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_take */"
  , "__attribute__ ((const))"
  , "struct uses_vendor (*hs_bindgen_c0843eceab67c54d (void)) ("
  , "  struct no_vendor const *arg1"
  , ")"
  , "{"
  , "  return &uses_vendor_take;"
  , "}"
  , "/* test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_age */"
  , "__attribute__ ((const))"
  , "vendor_typedef (*hs_bindgen_f4a618264fab0f1d (void)) ("
  , "  struct uses_vendor const *arg1"
  , ")"
  , "{"
  , "  return &uses_vendor_age;"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_take@
foreign import ccall unsafe "hs_bindgen_c0843eceab67c54d" hs_bindgen_c0843eceab67c54d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_take@
hs_bindgen_c0843eceab67c54d :: IO (BG.FunPtr (PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor -> IO Uses_vendor))
hs_bindgen_c0843eceab67c54d =
  fmap BG.fromFFIType hs_bindgen_c0843eceab67c54d_base

{-# NOINLINE uses_vendor_take #-}
{-| __C declaration:__ @uses_vendor_take@

    __defined at:__ @uses_vendor.h 16:20@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_take :: BG.FunPtr (PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor -> IO Uses_vendor)
uses_vendor_take =
  BG.unsafePerformIO hs_bindgen_c0843eceab67c54d

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_age@
foreign import ccall unsafe "hs_bindgen_f4a618264fab0f1d" hs_bindgen_f4a618264fab0f1d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_get_uses_vendor_age@
hs_bindgen_f4a618264fab0f1d :: IO (BG.FunPtr (PtrConst.PtrConst Uses_vendor -> IO VendorDependencies.Vendor.Vendor_typedef))
hs_bindgen_f4a618264fab0f1d =
  fmap BG.fromFFIType hs_bindgen_f4a618264fab0f1d_base

{-# NOINLINE uses_vendor_age #-}
{-| __C declaration:__ @uses_vendor_age@

    __defined at:__ @uses_vendor.h 17:16@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_age :: BG.FunPtr (PtrConst.PtrConst Uses_vendor -> IO VendorDependencies.Vendor.Vendor_typedef)
uses_vendor_age =
  BG.unsafePerformIO hs_bindgen_f4a618264fab0f1d
