{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Canvas.FunPtr
    ( Geometry.Canvas.FunPtr.canvas_draw
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Canvas
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Canvas_get_canvas_draw */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4d70e54c57a40e49 (void)) ("
  , "  struct canvas *arg1,"
  , "  struct shape const *arg2"
  , ")"
  , "{"
  , "  return &canvas_draw;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_get_canvas_draw@
foreign import ccall unsafe "hs_bindgen_4d70e54c57a40e49" hs_bindgen_4d70e54c57a40e49_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Canvas_get_canvas_draw@
hs_bindgen_4d70e54c57a40e49 :: IO (BG.FunPtr (BG.Ptr Canvas -> PtrConst.PtrConst Geometry.Shape.Shape -> IO ()))
hs_bindgen_4d70e54c57a40e49 =
  fmap BG.fromFFIType hs_bindgen_4d70e54c57a40e49_base

{-# NOINLINE canvas_draw #-}
{-| __C declaration:__ @canvas_draw@

    __defined at:__ @geometry\/canvas.h 15:6@

    __exported by:__ @import_cycle.h@
-}
canvas_draw :: BG.FunPtr (BG.Ptr Canvas -> PtrConst.PtrConst Geometry.Shape.Shape -> IO ())
canvas_draw =
  BG.unsafePerformIO hs_bindgen_4d70e54c57a40e49
