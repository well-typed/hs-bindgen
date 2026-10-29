{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Window.Safe
    ( M.Window.Safe.window_add
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Widget
import M.Window
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "void hs_bindgen_c0a45a2005a9f815 ("
  , "  struct window *arg1,"
  , "  struct widget *arg2"
  , ")"
  , "{"
  , "  (window_add)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_window_widget_M.Window_Safe_window_add@
foreign import ccall safe "hs_bindgen_c0a45a2005a9f815" hs_bindgen_c0a45a2005a9f815_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_window_widget_M.Window_Safe_window_add@
hs_bindgen_c0a45a2005a9f815 ::
     BG.Ptr Window
  -> BG.Ptr M.Widget.Widget
  -> IO ()
hs_bindgen_c0a45a2005a9f815 =
  \x0 ->
    \x1 ->
      hs_bindgen_c0a45a2005a9f815_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @window_add@

    __defined at:__ @window.h 6:6@

    __exported by:__ @root.h@
-}
window_add ::
     BG.Ptr Window
     -- ^ __C declaration:__ @w@
  -> BG.Ptr M.Widget.Widget
     -- ^ __C declaration:__ @child@
  -> IO ()
window_add = hs_bindgen_c0a45a2005a9f815
