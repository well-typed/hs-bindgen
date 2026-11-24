{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Io.Svg_export.Unsafe
    ( Geometry.Io.Svg_export.Unsafe.svg_export
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <follows_includes.h>"
  , "signed int hs_bindgen_a6deb76db10c5080 ("
  , "  struct shape const *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (svg_export)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Io.Svg_export_Unsafe_svg_export@
foreign import ccall unsafe "hs_bindgen_a6deb76db10c5080" hs_bindgen_a6deb76db10c5080_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Io.Svg_export_Unsafe_svg_export@
hs_bindgen_a6deb76db10c5080 ::
     PtrConst.PtrConst Geometry.Shape.Shape
  -> PtrConst.PtrConst BG.CChar
  -> IO BG.CInt
hs_bindgen_a6deb76db10c5080 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_a6deb76db10c5080_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @svg_export@

    __defined at:__ @geometry\/io\/svg-export.h 10:5@

    __exported by:__ @follows_includes.h@
-}
svg_export ::
     PtrConst.PtrConst Geometry.Shape.Shape
     -- ^ __C declaration:__ @shape@
  -> PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @path@
  -> IO BG.CInt
svg_export = hs_bindgen_a6deb76db10c5080
