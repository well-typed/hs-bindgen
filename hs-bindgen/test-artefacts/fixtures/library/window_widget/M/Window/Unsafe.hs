{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Window.Unsafe
    ( M.Window.Unsafe.window_add
    , M.Window.Unsafe.widget_window
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Window
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <window_widget/root.h>"
  , "void hs_bindgen_79614a8cd545a510 ("
  , "  struct window *arg1,"
  , "  struct widget *arg2"
  , ")"
  , "{"
  , "  (window_add)(arg1, arg2);"
  , "}"
  , "struct window *hs_bindgen_fb2f3bba1eecb000 ("
  , "  struct widget *arg1"
  , ")"
  , "{"
  , "  return (widget_window)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarywindow_widget_M.Window_Unsafe_window_add@
foreign import ccall unsafe "hs_bindgen_79614a8cd545a510" hs_bindgen_79614a8cd545a510_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywindow_widget_M.Window_Unsafe_window_add@
hs_bindgen_79614a8cd545a510 ::
     BG.Ptr Window
  -> BG.Ptr Widget
  -> IO ()
hs_bindgen_79614a8cd545a510 =
  \x0 ->
    \x1 ->
      hs_bindgen_79614a8cd545a510_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @window_add@

    __defined at:__ @window.h 12:6@

    __exported by:__ @window_widget\/root.h@
-}
window_add ::
     BG.Ptr Window
     -- ^ __C declaration:__ @w@
  -> BG.Ptr Widget
     -- ^ __C declaration:__ @child@
  -> IO ()
window_add = hs_bindgen_79614a8cd545a510

-- __unique:__ @test_librarywindow_widget_M.Window_Unsafe_widget_window@
foreign import ccall unsafe "hs_bindgen_fb2f3bba1eecb000" hs_bindgen_fb2f3bba1eecb000_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywindow_widget_M.Window_Unsafe_widget_window@
hs_bindgen_fb2f3bba1eecb000 ::
     BG.Ptr Widget
  -> IO (BG.Ptr Window)
hs_bindgen_fb2f3bba1eecb000 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fb2f3bba1eecb000_base (BG.toFFIType x0))

{-| __C declaration:__ @widget_window@

    __defined at:__ @widget.h 12:16@

    __exported by:__ @window_widget\/root.h@
-}
widget_window ::
     BG.Ptr Widget
     -- ^ __C declaration:__ @w@
  -> IO (BG.Ptr Window)
widget_window = hs_bindgen_fb2f3bba1eecb000
