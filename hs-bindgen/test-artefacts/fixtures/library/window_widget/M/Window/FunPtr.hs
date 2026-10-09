{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Window.FunPtr
    ( M.Window.FunPtr.window_add
    , M.Window.FunPtr.widget_window
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Window
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <window_widget/root.h>"
  , "/* test_librarywindow_widget_M.Window_get_window_add */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_410563680dd06ea2 (void)) ("
  , "  struct window *arg1,"
  , "  struct widget *arg2"
  , ")"
  , "{"
  , "  return &window_add;"
  , "}"
  , "/* test_librarywindow_widget_M.Window_get_widget_window */"
  , "__attribute__ ((const))"
  , "struct window *(*hs_bindgen_5c81b55e1adf0589 (void)) ("
  , "  struct widget *arg1"
  , ")"
  , "{"
  , "  return &widget_window;"
  , "}"
  ]))

-- __unique:__ @test_librarywindow_widget_M.Window_get_window_add@
foreign import ccall unsafe "hs_bindgen_410563680dd06ea2" hs_bindgen_410563680dd06ea2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywindow_widget_M.Window_get_window_add@
hs_bindgen_410563680dd06ea2 :: IO (BG.FunPtr (BG.Ptr Window -> BG.Ptr Widget -> IO ()))
hs_bindgen_410563680dd06ea2 =
  fmap BG.fromFFIType hs_bindgen_410563680dd06ea2_base

{-# NOINLINE window_add #-}
{-| __C declaration:__ @window_add@

    __defined at:__ @window.h 12:6@

    __exported by:__ @window_widget\/root.h@
-}
window_add :: BG.FunPtr (BG.Ptr Window -> BG.Ptr Widget -> IO ())
window_add =
  BG.unsafePerformIO hs_bindgen_410563680dd06ea2

-- __unique:__ @test_librarywindow_widget_M.Window_get_widget_window@
foreign import ccall unsafe "hs_bindgen_5c81b55e1adf0589" hs_bindgen_5c81b55e1adf0589_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywindow_widget_M.Window_get_widget_window@
hs_bindgen_5c81b55e1adf0589 :: IO (BG.FunPtr (BG.Ptr Widget -> IO (BG.Ptr Window)))
hs_bindgen_5c81b55e1adf0589 =
  fmap BG.fromFFIType hs_bindgen_5c81b55e1adf0589_base

{-# NOINLINE widget_window #-}
{-| __C declaration:__ @widget_window@

    __defined at:__ @widget.h 12:16@

    __exported by:__ @window_widget\/root.h@
-}
widget_window :: BG.FunPtr (BG.Ptr Widget -> IO (BG.Ptr Window))
widget_window =
  BG.unsafePerformIO hs_bindgen_5c81b55e1adf0589
