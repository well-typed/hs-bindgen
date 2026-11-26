{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Point.FunPtr
    ( Geometry.Point.FunPtr.point_distance
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Point
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Point_get_point_distance */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_41aede0b4d8332cc (void)) ("
  , "  struct point const *arg1,"
  , "  struct point const *arg2"
  , ")"
  , "{"
  , "  return &point_distance;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Point_get_point_distance@
foreign import ccall unsafe "hs_bindgen_41aede0b4d8332cc" hs_bindgen_41aede0b4d8332cc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Point_get_point_distance@
hs_bindgen_41aede0b4d8332cc :: IO (BG.FunPtr (PtrConst.PtrConst Point -> PtrConst.PtrConst Point -> IO BG.CDouble))
hs_bindgen_41aede0b4d8332cc =
  fmap BG.fromFFIType hs_bindgen_41aede0b4d8332cc_base

{-# NOINLINE point_distance #-}
{-| __C declaration:__ @point_distance@

    __defined at:__ @geometry\/point.h 14:8@

    __exported by:__ @import_cycle.h@
-}
point_distance :: BG.FunPtr (PtrConst.PtrConst Point -> PtrConst.PtrConst Point -> IO BG.CDouble)
point_distance =
  BG.unsafePerformIO hs_bindgen_41aede0b4d8332cc
