{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Window.Safe
    ( M.Window.Safe.window_add
    , M.Window.Safe.widget_window
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Window
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <window_widget/root.h>"
  , "void hs_bindgen_c493cb371fb476b1 ("
  , "  struct window *arg1,"
  , "  struct widget *arg2"
  , ")"
  , "{"
  , "  (window_add)(arg1, arg2);"
  , "}"
  , "struct window *hs_bindgen_6ef44549a97ca02c ("
  , "  struct widget *arg1"
  , ")"
  , "{"
  , "  return (widget_window)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarywindow_widget_M.Window_Safe_window_add@
foreign import ccall safe "hs_bindgen_c493cb371fb476b1" hs_bindgen_c493cb371fb476b1_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywindow_widget_M.Window_Safe_window_add@
hs_bindgen_c493cb371fb476b1 ::
     BG.Ptr Window
  -> BG.Ptr Widget
  -> IO ()
hs_bindgen_c493cb371fb476b1 =
  \x0 ->
    \x1 ->
      hs_bindgen_c493cb371fb476b1_base (BG.toFFIType x0) (BG.toFFIType x1)

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
window_add = hs_bindgen_c493cb371fb476b1

-- __unique:__ @test_librarywindow_widget_M.Window_Safe_widget_window@
foreign import ccall safe "hs_bindgen_6ef44549a97ca02c" hs_bindgen_6ef44549a97ca02c_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywindow_widget_M.Window_Safe_widget_window@
hs_bindgen_6ef44549a97ca02c ::
     BG.Ptr Widget
  -> IO (BG.Ptr Window)
hs_bindgen_6ef44549a97ca02c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_6ef44549a97ca02c_base (BG.toFFIType x0))

{-| __C declaration:__ @widget_window@

    __defined at:__ @widget.h 12:16@

    __exported by:__ @window_widget\/root.h@
-}
widget_window ::
     BG.Ptr Widget
     -- ^ __C declaration:__ @w@
  -> IO (BG.Ptr Window)
widget_window = hs_bindgen_6ef44549a97ca02c
