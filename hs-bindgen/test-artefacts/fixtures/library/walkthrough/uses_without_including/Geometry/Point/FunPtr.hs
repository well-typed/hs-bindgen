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
  [ "#include <uses_without_including.h>"
  , "/* test_librarywalkthroughuses_witho_Geometry.Point_get_point_distance */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_93a1753922d20d09 (void)) ("
  , "  struct point const *arg1,"
  , "  struct point const *arg2"
  , ")"
  , "{"
  , "  return &point_distance;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Point_get_point_distance@
foreign import ccall unsafe "hs_bindgen_93a1753922d20d09" hs_bindgen_93a1753922d20d09_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Point_get_point_distance@
hs_bindgen_93a1753922d20d09 :: IO (BG.FunPtr (PtrConst.PtrConst Point -> PtrConst.PtrConst Point -> IO BG.CDouble))
hs_bindgen_93a1753922d20d09 =
  fmap BG.fromFFIType hs_bindgen_93a1753922d20d09_base

{-# NOINLINE point_distance #-}
{-| __C declaration:__ @point_distance@

    __defined at:__ @geometry\/point.h 14:8@

    __exported by:__ @uses_without_including.h@
-}
point_distance :: BG.FunPtr (PtrConst.PtrConst Point -> PtrConst.PtrConst Point -> IO BG.CDouble)
point_distance =
  BG.unsafePerformIO hs_bindgen_93a1753922d20d09
