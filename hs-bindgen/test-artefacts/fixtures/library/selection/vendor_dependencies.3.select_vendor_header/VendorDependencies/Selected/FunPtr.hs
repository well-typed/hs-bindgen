{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module VendorDependencies.Selected.FunPtr
    ( VendorDependencies.Selected.FunPtr.selected_apply
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)
import VendorDependencies.Selected

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_dependencies.h>"
  , "/* test_libraryselectionvendor_depen_VendorDependencies.Selected_get_selected_apply */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_778a92f5fc3f8b22 (void)) ("
  , "  struct selected const *arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &selected_apply;"
  , "}"
  ]))

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Selected_get_selected_apply@
foreign import ccall unsafe "hs_bindgen_778a92f5fc3f8b22" hs_bindgen_778a92f5fc3f8b22_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryselectionvendor_depen_VendorDependencies.Selected_get_selected_apply@
hs_bindgen_778a92f5fc3f8b22 :: IO (BG.FunPtr (PtrConst.PtrConst Selected -> BG.CInt -> IO BG.CDouble))
hs_bindgen_778a92f5fc3f8b22 =
  fmap BG.fromFFIType hs_bindgen_778a92f5fc3f8b22_base

{-# NOINLINE selected_apply #-}
{-| __C declaration:__ @selected_apply@

    __defined at:__ @vendor_dependencies\/selected.h 13:8@

    __exported by:__ @vendor_dependencies.h@
-}
selected_apply :: BG.FunPtr (PtrConst.PtrConst Selected -> BG.CInt -> IO BG.CDouble)
selected_apply =
  BG.unsafePerformIO hs_bindgen_778a92f5fc3f8b22
