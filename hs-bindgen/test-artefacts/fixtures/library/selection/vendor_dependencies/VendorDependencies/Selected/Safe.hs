{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Selected.Safe
    ( VendorDependencies.Selected.Safe.selected_apply
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.Selected

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "double hs_bindgen_5704535240c619c6 ("
  , "  struct selected const *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return (selected_apply)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Selected_Safe_selected_apply@
foreign import ccall safe "hs_bindgen_5704535240c619c6" hs_bindgen_5704535240c619c6_base ::
     BG.Ptr BG.Void
  -> BG.CInt
  -> IO BG.CDouble

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Selected_Safe_selected_apply@
hs_bindgen_5704535240c619c6 ::
     PtrConst.PtrConst Selected
  -> BG.CInt
  -> IO BG.CDouble
hs_bindgen_5704535240c619c6 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_5704535240c619c6_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @selected_apply@

    __defined at:__ @vendor_dependencies\/selected.h 13:8@

    __exported by:__ @vendor_dependencies.h@
-}
selected_apply ::
     PtrConst.PtrConst Selected
     -- ^ __C declaration:__ @c@
  -> BG.CInt
     -- ^ __C declaration:__ @raw@
  -> IO BG.CDouble
selected_apply = hs_bindgen_5704535240c619c6
