{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Point.Global
    ( Geometry.Point.Global.point_origin
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Point
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <follows_includes.h>"
  , "/* test_librarywalkthroughfollows_in_Geometry.Point_get_point_origin */"
  , "__attribute__ ((const))"
  , "struct point const *hs_bindgen_8515836f7e9bb01d (void)"
  , "{"
  , "  return &point_origin;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Point_get_point_origin@
foreign import ccall unsafe "hs_bindgen_8515836f7e9bb01d" hs_bindgen_8515836f7e9bb01d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Point_get_point_origin@
hs_bindgen_8515836f7e9bb01d :: IO (PtrConst.PtrConst Point)
hs_bindgen_8515836f7e9bb01d =
  fmap BG.fromFFIType hs_bindgen_8515836f7e9bb01d_base

{-# NOINLINE hs_bindgen_3820a8a5f9a26f5c #-}
{-| __C declaration:__ @point_origin@

    __defined at:__ @geometry\/point.h 12:27@

    __exported by:__ @follows_includes.h@

    __unique:__ @test_librarywalkthroughfollows_in_Geometry.Point_point_origin@
-}
hs_bindgen_3820a8a5f9a26f5c :: PtrConst.PtrConst Point
hs_bindgen_3820a8a5f9a26f5c =
  BG.unsafePerformIO hs_bindgen_8515836f7e9bb01d

{-# NOINLINE point_origin #-}
point_origin :: Point
point_origin =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_3820a8a5f9a26f5c)
