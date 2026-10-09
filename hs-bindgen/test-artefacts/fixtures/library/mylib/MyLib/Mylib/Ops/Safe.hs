{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Ops.Safe
    ( MyLib.Mylib.Ops.Safe.point_distance
    , MyLib.Mylib.Ops.Safe.rect_area
    , MyLib.Mylib.Ops.Safe.rect_contains
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "double hs_bindgen_654d56e4314c4bc5 ("
  , "  point_t *arg1,"
  , "  point_t *arg2"
  , ")"
  , "{"
  , "  return (point_distance)(*arg1, *arg2);"
  , "}"
  , "double hs_bindgen_7a2663780746ddcf ("
  , "  rect_t *arg1"
  , ")"
  , "{"
  , "  return (rect_area)(*arg1);"
  , "}"
  , "signed int hs_bindgen_1bff64c3e77ac0f6 ("
  , "  rect_t *arg1,"
  , "  point_t *arg2"
  , ")"
  , "{"
  , "  return (rect_contains)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_point_distance@
foreign import ccall safe "hs_bindgen_654d56e4314c4bc5" hs_bindgen_654d56e4314c4bc5_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CDouble

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_point_distance@
hs_bindgen_654d56e4314c4bc5 ::
     BG.Ptr MyLib.Mylib.Types.Point_t
  -> BG.Ptr MyLib.Mylib.Types.Point_t
  -> IO BG.CDouble
hs_bindgen_654d56e4314c4bc5 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_654d56e4314c4bc5_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @point_distance@

    __defined at:__ @mylib\/ops.h 4:8@

    __exported by:__ @mylib.h@
-}
point_distance ::
     MyLib.Mylib.Types.Point_t
     -- ^ __C declaration:__ @a@
  -> MyLib.Mylib.Types.Point_t
     -- ^ __C declaration:__ @b@
  -> IO BG.CDouble
point_distance =
  \a0 ->
    \b1 ->
      BG.with a0 (\a2 ->
                    BG.with b1 (\b3 ->
                                  hs_bindgen_654d56e4314c4bc5 a2 b3))

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_rect_area@
foreign import ccall safe "hs_bindgen_7a2663780746ddcf" hs_bindgen_7a2663780746ddcf_base ::
     BG.Ptr BG.Void
  -> IO BG.CDouble

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_rect_area@
hs_bindgen_7a2663780746ddcf ::
     BG.Ptr MyLib.Mylib.Types.Rect_t
  -> IO BG.CDouble
hs_bindgen_7a2663780746ddcf =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7a2663780746ddcf_base (BG.toFFIType x0))

{-| __C declaration:__ @rect_area@

    __defined at:__ @mylib\/ops.h 5:8@

    __exported by:__ @mylib.h@
-}
rect_area ::
     MyLib.Mylib.Types.Rect_t
     -- ^ __C declaration:__ @r@
  -> IO BG.CDouble
rect_area =
  \r0 ->
    BG.with r0 (\r1 -> hs_bindgen_7a2663780746ddcf r1)

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_rect_contains@
foreign import ccall safe "hs_bindgen_1bff64c3e77ac0f6" hs_bindgen_1bff64c3e77ac0f6_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_librarymylib_MyLib.Mylib.Ops_Safe_rect_contains@
hs_bindgen_1bff64c3e77ac0f6 ::
     BG.Ptr MyLib.Mylib.Types.Rect_t
  -> BG.Ptr MyLib.Mylib.Types.Point_t
  -> IO BG.CInt
hs_bindgen_1bff64c3e77ac0f6 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_1bff64c3e77ac0f6_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @rect_contains@

    __defined at:__ @mylib\/ops.h 6:5@

    __exported by:__ @mylib.h@
-}
rect_contains ::
     MyLib.Mylib.Types.Rect_t
     -- ^ __C declaration:__ @r@
  -> MyLib.Mylib.Types.Point_t
     -- ^ __C declaration:__ @p@
  -> IO BG.CInt
rect_contains =
  \r0 ->
    \p1 ->
      BG.with r0 (\r2 ->
                    BG.with p1 (\p3 ->
                                  hs_bindgen_1bff64c3e77ac0f6 r2 p3))
