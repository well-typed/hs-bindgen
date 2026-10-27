{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Widget.Unsafe
    ( M.Widget.Unsafe.widget_window
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Window
import M.Widget
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "struct window *hs_bindgen_ae934eb909f03109 ("
  , "  struct widget *arg1"
  , ")"
  , "{"
  , "  return (widget_window)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_window_widget_M.Widget_Unsafe_widget_window@
foreign import ccall unsafe "hs_bindgen_ae934eb909f03109" hs_bindgen_ae934eb909f03109_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_window_widget_M.Widget_Unsafe_widget_window@
hs_bindgen_ae934eb909f03109 ::
     BG.Ptr Widget
  -> IO (BG.Ptr M.Window.Window)
hs_bindgen_ae934eb909f03109 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_ae934eb909f03109_base (BG.toFFIType x0))

{-| __C declaration:__ @widget_window@

    __defined at:__ @widget.h 6:16@

    __exported by:__ @root.h@
-}
widget_window ::
     BG.Ptr Widget
     -- ^ __C declaration:__ @w@
  -> IO (BG.Ptr M.Window.Window)
widget_window = hs_bindgen_ae934eb909f03109
