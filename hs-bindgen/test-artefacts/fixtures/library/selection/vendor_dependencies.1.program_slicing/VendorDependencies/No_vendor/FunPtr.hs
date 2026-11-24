{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.No_vendor.FunPtr
    ( VendorDependencies.No_vendor.FunPtr.no_vendor_read
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.No_vendor

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "/* test_libraryselectionvendor_depen_VendorDependencies.No_vendor_get_no_vendor_read */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c43dd6a2f72b313b (void)) ("
  , "  struct no_vendor const *arg1"
  , ")"
  , "{"
  , "  return &no_vendor_read;"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.No_vendor_get_no_vendor_read@
foreign import ccall unsafe "hs_bindgen_c43dd6a2f72b313b" hs_bindgen_c43dd6a2f72b313b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.No_vendor_get_no_vendor_read@
hs_bindgen_c43dd6a2f72b313b :: IO (BG.FunPtr (PtrConst.PtrConst No_vendor -> IO BG.CInt))
hs_bindgen_c43dd6a2f72b313b =
  fmap BG.fromFFIType hs_bindgen_c43dd6a2f72b313b_base

{-# NOINLINE no_vendor_read #-}
{-| __C declaration:__ @no_vendor_read@

    __defined at:__ @no_vendor.h 9:5@

    __exported by:__ @vendor_dependencies.h@
-}
no_vendor_read :: BG.FunPtr (PtrConst.PtrConst No_vendor -> IO BG.CInt)
no_vendor_read =
  BG.unsafePerformIO hs_bindgen_c43dd6a2f72b313b
