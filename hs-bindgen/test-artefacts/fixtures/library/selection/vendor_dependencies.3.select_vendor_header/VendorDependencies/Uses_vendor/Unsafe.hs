{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Uses_vendor.Unsafe
    ( VendorDependencies.Uses_vendor.Unsafe.uses_vendor_take
    , VendorDependencies.Uses_vendor.Unsafe.uses_vendor_age
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
  , "void hs_bindgen_d454a2fc6a22beb1 ("
  , "  struct no_vendor const *arg1,"
  , "  struct uses_vendor *arg2"
  , ")"
  , "{"
  , "  *arg2 = (uses_vendor_take)(arg1);"
  , "}"
  , "vendor_typedef hs_bindgen_d0897573d6c07828 ("
  , "  struct uses_vendor const *arg1"
  , ")"
  , "{"
  , "  return (uses_vendor_age)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_Unsafe_uses_vendor_take@
foreign import ccall unsafe "hs_bindgen_d454a2fc6a22beb1" hs_bindgen_d454a2fc6a22beb1_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_Unsafe_uses_vendor_take@
hs_bindgen_d454a2fc6a22beb1 ::
     PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
  -> BG.Ptr Uses_vendor
  -> IO ()
hs_bindgen_d454a2fc6a22beb1 =
  \x0 ->
    \x1 ->
      hs_bindgen_d454a2fc6a22beb1_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @uses_vendor_take@

    __defined at:__ @uses_vendor.h 16:20@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_take ::
     PtrConst.PtrConst VendorDependencies.No_vendor.No_vendor
     -- ^ __C declaration:__ @s@
  -> IO Uses_vendor
uses_vendor_take =
  \s0 ->
    BG.allocaAndPeek (\res1 ->
                        hs_bindgen_d454a2fc6a22beb1 s0 res1)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_Unsafe_uses_vendor_age@
foreign import ccall unsafe "hs_bindgen_d0897573d6c07828" hs_bindgen_d0897573d6c07828_base ::
     BG.Ptr BG.Void
  -> IO BG.CLong

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_Unsafe_uses_vendor_age@
hs_bindgen_d0897573d6c07828 ::
     PtrConst.PtrConst Uses_vendor
  -> IO VendorDependencies.Vendor.Vendor_typedef
hs_bindgen_d0897573d6c07828 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_d0897573d6c07828_base (BG.toFFIType x0))

{-| __C declaration:__ @uses_vendor_age@

    __defined at:__ @uses_vendor.h 17:16@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_age ::
     PtrConst.PtrConst Uses_vendor
     -- ^ __C declaration:__ @s@
  -> IO VendorDependencies.Vendor.Vendor_typedef
uses_vendor_age = hs_bindgen_d0897573d6c07828
