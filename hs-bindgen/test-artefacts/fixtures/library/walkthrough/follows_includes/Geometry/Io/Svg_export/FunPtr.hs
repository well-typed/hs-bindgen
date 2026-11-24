{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Io.Svg_export.FunPtr
    ( Geometry.Io.Svg_export.FunPtr.svg_export
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <follows_includes.h>"
  , "/* test_librarywalkthroughfollows_in_Geometry.Io.Svg_export_get_svg_export */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_84688b8af710adb5 (void)) ("
  , "  struct shape const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &svg_export;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Io.Svg_export_get_svg_export@
foreign import ccall unsafe "hs_bindgen_84688b8af710adb5" hs_bindgen_84688b8af710adb5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Io.Svg_export_get_svg_export@
hs_bindgen_84688b8af710adb5 :: IO (BG.FunPtr (PtrConst.PtrConst Geometry.Shape.Shape -> PtrConst.PtrConst BG.CChar -> IO BG.CInt))
hs_bindgen_84688b8af710adb5 =
  fmap BG.fromFFIType hs_bindgen_84688b8af710adb5_base

{-# NOINLINE svg_export #-}
{-| __C declaration:__ @svg_export@

    __defined at:__ @geometry\/io\/svg-export.h 10:5@

    __exported by:__ @follows_includes.h@
-}
svg_export :: BG.FunPtr (PtrConst.PtrConst Geometry.Shape.Shape -> PtrConst.PtrConst BG.CChar -> IO BG.CInt)
svg_export =
  BG.unsafePerformIO hs_bindgen_84688b8af710adb5
