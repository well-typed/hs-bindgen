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
  [ "#include <import_cycle.h>"
  , "/* test_librarywalkthroughimport_cyc_Geometry.Point_get_point_origin */"
  , "__attribute__ ((const))"
  , "struct point const *hs_bindgen_4ea46a062e1205bf (void)"
  , "{"
  , "  return &point_origin;"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Point_get_point_origin@
foreign import ccall unsafe "hs_bindgen_4ea46a062e1205bf" hs_bindgen_4ea46a062e1205bf_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Point_get_point_origin@
hs_bindgen_4ea46a062e1205bf :: IO (PtrConst.PtrConst Point)
hs_bindgen_4ea46a062e1205bf =
  fmap BG.fromFFIType hs_bindgen_4ea46a062e1205bf_base

{-# NOINLINE hs_bindgen_e5ad5572f37ec98e #-}
{-| __C declaration:__ @point_origin@

    __defined at:__ @geometry\/point.h 12:27@

    __exported by:__ @import_cycle.h@

    __unique:__ @test_librarywalkthroughimport_cyc_Geometry.Point_point_origin@
-}
hs_bindgen_e5ad5572f37ec98e :: PtrConst.PtrConst Point
hs_bindgen_e5ad5572f37ec98e =
  BG.unsafePerformIO hs_bindgen_4ea46a062e1205bf

{-# NOINLINE point_origin #-}
point_origin :: Point
point_origin =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_e5ad5572f37ec98e)
