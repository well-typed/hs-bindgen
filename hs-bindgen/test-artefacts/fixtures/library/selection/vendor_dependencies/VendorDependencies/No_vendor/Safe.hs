{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.No_vendor.Safe
    ( VendorDependencies.No_vendor.Safe.no_vendor_read
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.No_vendor

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "signed int hs_bindgen_4dc68198d9a2029b ("
  , "  struct no_vendor const *arg1"
  , ")"
  , "{"
  , "  return (no_vendor_read)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.No_vendor_Safe_no_vendor_read@
foreign import ccall safe "hs_bindgen_4dc68198d9a2029b" hs_bindgen_4dc68198d9a2029b_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.No_vendor_Safe_no_vendor_read@
hs_bindgen_4dc68198d9a2029b ::
     PtrConst.PtrConst No_vendor
  -> IO BG.CInt
hs_bindgen_4dc68198d9a2029b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4dc68198d9a2029b_base (BG.toFFIType x0))

{-| __C declaration:__ @no_vendor_read@

    __defined at:__ @no_vendor.h 9:5@

    __exported by:__ @vendor_dependencies.h@
-}
no_vendor_read ::
     PtrConst.PtrConst No_vendor
     -- ^ __C declaration:__ @s@
  -> IO BG.CInt
no_vendor_read = hs_bindgen_4dc68198d9a2029b
