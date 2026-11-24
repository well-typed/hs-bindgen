{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Canvas.Unsafe
    ( Geometry.Canvas.Unsafe.canvas_draw
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
  , "void hs_bindgen_45bc0ea1b8e70c0e ("
  , "  struct canvas *arg1,"
  , "  struct shape const *arg2"
  , ")"
  , "{"
  , "  (canvas_draw)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_Unsafe_canvas_draw@
foreign import ccall unsafe "hs_bindgen_45bc0ea1b8e70c0e" hs_bindgen_45bc0ea1b8e70c0e_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_Unsafe_canvas_draw@
hs_bindgen_45bc0ea1b8e70c0e ::
     BG.Ptr Canvas
  -> PtrConst.PtrConst Geometry.Shape.Shape
  -> IO ()
hs_bindgen_45bc0ea1b8e70c0e =
  \x0 ->
    \x1 ->
      hs_bindgen_45bc0ea1b8e70c0e_base (BG.toFFIType x0) (BG.toFFIType x1)

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
canvas_draw = hs_bindgen_45bc0ea1b8e70c0e
