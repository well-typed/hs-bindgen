{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Uses_vendor_indirectly.Unsafe
    ( VendorDependencies.Uses_vendor_indirectly.Unsafe.uses_vendor_indirectly_apply
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.Uses_vendor_indirectly

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "double hs_bindgen_e53470b6b7e9cc8c ("
  , "  struct uses_vendor_indirectly *arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (uses_vendor_indirectly_apply)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_indirectly_Unsafe_uses_vendor_indirectly_apply@
foreign import ccall unsafe "hs_bindgen_e53470b6b7e9cc8c" hs_bindgen_e53470b6b7e9cc8c_base ::
     BG.Ptr BG.Void
  -> BG.CDouble
  -> IO BG.CDouble

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Uses_vendor_indirectly_Unsafe_uses_vendor_indirectly_apply@
hs_bindgen_e53470b6b7e9cc8c ::
     BG.Ptr Uses_vendor_indirectly
  -> BG.CDouble
  -> IO BG.CDouble
hs_bindgen_e53470b6b7e9cc8c =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_e53470b6b7e9cc8c_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @uses_vendor_indirectly_apply@

    __defined at:__ @vendor_dependencies\/uses_vendor_indirectly.h 12:8@

    __exported by:__ @vendor_dependencies.h@
-}
uses_vendor_indirectly_apply ::
     BG.Ptr Uses_vendor_indirectly
     -- ^ __C declaration:__ @f@
  -> BG.CDouble
     -- ^ __C declaration:__ @value@
  -> IO BG.CDouble
uses_vendor_indirectly_apply =
  hs_bindgen_e53470b6b7e9cc8c
