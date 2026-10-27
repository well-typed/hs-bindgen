{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Widget.FunPtr
    ( M.Widget.FunPtr.widget_window
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Window
import M.Widget
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_window_widget_M.Widget_get_widget_window */"
  , "__attribute__ ((const))"
  , "struct window *(*hs_bindgen_bc48c2ac8ce6f1ea (void)) ("
  , "  struct widget *arg1"
  , ")"
  , "{"
  , "  return &widget_window;"
  , "}"
  ]))

-- __unique:__ @test_window_widget_M.Widget_get_widget_window@
foreign import ccall unsafe "hs_bindgen_bc48c2ac8ce6f1ea" hs_bindgen_bc48c2ac8ce6f1ea_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_window_widget_M.Widget_get_widget_window@
hs_bindgen_bc48c2ac8ce6f1ea :: IO (BG.FunPtr (BG.Ptr Widget -> IO (BG.Ptr M.Window.Window)))
hs_bindgen_bc48c2ac8ce6f1ea =
  fmap BG.fromFFIType hs_bindgen_bc48c2ac8ce6f1ea_base

{-# NOINLINE widget_window #-}
{-| __C declaration:__ @widget_window@

    __defined at:__ @widget.h 6:16@

    __exported by:__ @root.h@
-}
widget_window :: BG.FunPtr (BG.Ptr Widget -> IO (BG.Ptr M.Window.Window))
widget_window =
  BG.unsafePerformIO hs_bindgen_bc48c2ac8ce6f1ea
