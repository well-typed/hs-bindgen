{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.User.Unsafe
    ( M.User.Unsafe.window_show
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Alias
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <vendor_by_tag/root.h>"
  , "void hs_bindgen_048b7c1f6c637c5c ("
  , "  struct vendor_window *arg1"
  , ")"
  , "{"
  , "  (window_show)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryvendor_by_tag_M.User_Unsafe_window_show@
foreign import ccall unsafe "hs_bindgen_048b7c1f6c637c5c" hs_bindgen_048b7c1f6c637c5c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_libraryvendor_by_tag_M.User_Unsafe_window_show@
hs_bindgen_048b7c1f6c637c5c ::
     BG.Ptr M.Alias.Vendor_window
  -> IO ()
hs_bindgen_048b7c1f6c637c5c =
  \x0 ->
    hs_bindgen_048b7c1f6c637c5c_base (BG.toFFIType x0)

{-| __C declaration:__ @window_show@

    __defined at:__ @lib\/user.h 7:6@

    __exported by:__ @vendor_by_tag\/root.h@
-}
window_show ::
     BG.Ptr M.Alias.Vendor_window
     -- ^ __C declaration:__ @w@
  -> IO ()
window_show = hs_bindgen_048b7c1f6c637c5c
