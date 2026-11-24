{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Point.Safe
    ( Geometry.Point.Safe.point_distance
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Point
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <follows_includes.h>"
  , "double hs_bindgen_3a9befc4feb46e6e ("
  , "  struct point const *arg1,"
  , "  struct point const *arg2"
  , ")"
  , "{"
  , "  return (point_distance)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Point_Safe_point_distance@
foreign import ccall safe "hs_bindgen_3a9befc4feb46e6e" hs_bindgen_3a9befc4feb46e6e_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CDouble

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Point_Safe_point_distance@
hs_bindgen_3a9befc4feb46e6e ::
     PtrConst.PtrConst Point
  -> PtrConst.PtrConst Point
  -> IO BG.CDouble
hs_bindgen_3a9befc4feb46e6e =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_3a9befc4feb46e6e_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @point_distance@

    __defined at:__ @geometry\/point.h 14:8@

    __exported by:__ @follows_includes.h@
-}
point_distance ::
     PtrConst.PtrConst Point
     -- ^ __C declaration:__ @a@
  -> PtrConst.PtrConst Point
     -- ^ __C declaration:__ @b@
  -> IO BG.CDouble
point_distance = hs_bindgen_3a9befc4feb46e6e
