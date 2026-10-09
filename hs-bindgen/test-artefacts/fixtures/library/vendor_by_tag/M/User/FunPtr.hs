{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.User.FunPtr
    ( M.User.FunPtr.window_show
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Alias
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_by_tag/root.h>"
  , "/* test_libraryvendor_by_tag_M.User_get_window_show */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_eebba19fa13d8ea8 (void)) ("
  , "  struct vendor_window *arg1"
  , ")"
  , "{"
  , "  return &window_show;"
  , "}"
  ]))

-- __unique:__ @test_libraryvendor_by_tag_M.User_get_window_show@
foreign import ccall unsafe "hs_bindgen_eebba19fa13d8ea8" hs_bindgen_eebba19fa13d8ea8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryvendor_by_tag_M.User_get_window_show@
hs_bindgen_eebba19fa13d8ea8 :: IO (BG.FunPtr (BG.Ptr M.Alias.Vendor_window -> IO ()))
hs_bindgen_eebba19fa13d8ea8 =
  fmap BG.fromFFIType hs_bindgen_eebba19fa13d8ea8_base

{-# NOINLINE window_show #-}
{-| __C declaration:__ @window_show@

    __defined at:__ @lib\/user.h 7:6@

    __exported by:__ @vendor_by_tag\/root.h@
-}
window_show :: BG.FunPtr (BG.Ptr M.Alias.Vendor_window -> IO ())
window_show =
  BG.unsafePerformIO hs_bindgen_eebba19fa13d8ea8
