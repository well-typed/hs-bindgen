{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Window.FunPtr
    ( M.Window.FunPtr.window_add
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Widget
import M.Window
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_window_widget_M.Window_get_window_add */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c2c6173ac55462e9 (void)) ("
  , "  struct window *arg1,"
  , "  struct widget *arg2"
  , ")"
  , "{"
  , "  return &window_add;"
  , "}"
  ]))

-- __unique:__ @test_window_widget_M.Window_get_window_add@
foreign import ccall unsafe "hs_bindgen_c2c6173ac55462e9" hs_bindgen_c2c6173ac55462e9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_window_widget_M.Window_get_window_add@
hs_bindgen_c2c6173ac55462e9 :: IO (BG.FunPtr (BG.Ptr Window -> BG.Ptr M.Widget.Widget -> IO ()))
hs_bindgen_c2c6173ac55462e9 =
  fmap BG.fromFFIType hs_bindgen_c2c6173ac55462e9_base

{-# NOINLINE window_add #-}
{-| __C declaration:__ @window_add@

    __defined at:__ @window.h 6:6@

    __exported by:__ @root.h@
-}
window_add :: BG.FunPtr (BG.Ptr Window -> BG.Ptr M.Widget.Widget -> IO ())
window_add =
  BG.unsafePerformIO hs_bindgen_c2c6173ac55462e9
