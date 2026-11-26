{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Canvas.Safe
    ( Geometry.Canvas.Safe.canvas_draw
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Canvas
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "void hs_bindgen_04a06f40c5264592 ("
  , "  struct canvas *arg1,"
  , "  struct shape const *arg2"
  , ")"
  , "{"
  , "  (canvas_draw)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_Safe_canvas_draw@
foreign import ccall safe "hs_bindgen_04a06f40c5264592" hs_bindgen_04a06f40c5264592_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_Safe_canvas_draw@
hs_bindgen_04a06f40c5264592 ::
     BG.Ptr Canvas
  -> PtrConst.PtrConst Geometry.Shape.Shape
  -> IO ()
hs_bindgen_04a06f40c5264592 =
  \x0 ->
    \x1 ->
      hs_bindgen_04a06f40c5264592_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @canvas_draw@

    __defined at:__ @geometry\/canvas.h 15:6@

    __exported by:__ @import_cycle.h@
-}
canvas_draw ::
     BG.Ptr Canvas
     -- ^ __C declaration:__ @canvas@
  -> PtrConst.PtrConst Geometry.Shape.Shape
     -- ^ __C declaration:__ @shape@
  -> IO ()
canvas_draw = hs_bindgen_04a06f40c5264592
