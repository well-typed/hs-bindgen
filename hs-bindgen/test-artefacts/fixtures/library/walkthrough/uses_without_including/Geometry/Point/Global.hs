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
  [ "#include <uses_without_including.h>"
  , "/* test_librarywalkthroughuses_witho_Geometry.Point_get_point_origin */"
  , "__attribute__ ((const))"
  , "struct point const *hs_bindgen_238ec7dceb765eee (void)"
  , "{"
  , "  return &point_origin;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Point_get_point_origin@
foreign import ccall unsafe "hs_bindgen_238ec7dceb765eee" hs_bindgen_238ec7dceb765eee_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywalkthroughuses_witho_Geometry.Point_get_point_origin@
hs_bindgen_238ec7dceb765eee :: IO (PtrConst.PtrConst Point)
hs_bindgen_238ec7dceb765eee =
  fmap BG.fromFFIType hs_bindgen_238ec7dceb765eee_base

{-# NOINLINE hs_bindgen_e5361a6e2890d5c0 #-}
{-| __C declaration:__ @point_origin@

    __defined at:__ @geometry\/point.h 12:27@

    __exported by:__ @uses_without_including.h@

    __unique:__ @test_librarywalkthroughuses_witho_Geometry.Point_point_origin@
-}
hs_bindgen_e5361a6e2890d5c0 :: PtrConst.PtrConst Point
hs_bindgen_e5361a6e2890d5c0 =
  BG.unsafePerformIO hs_bindgen_238ec7dceb765eee

{-# NOINLINE point_origin #-}
point_origin :: Point
point_origin =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_e5361a6e2890d5c0)
