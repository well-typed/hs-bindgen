{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Geometry.Shape.Unsafe
    ( Geometry.Shape.Unsafe.shape_new
    , Geometry.Shape.Unsafe.shape_count_matching
    , Geometry.Shape.Unsafe.shape_for_each_point
    )
  where

import qualified Geometry.Point
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Geometry.Shape
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <follows_includes.h>"
  , "struct shape *hs_bindgen_eab5b423a48bd972 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (shape_new)(arg1);"
  , "}"
  , "signed int hs_bindgen_8ef0c1f8cefa7985 ("
  , "  struct shape **arg1,"
  , "  signed int arg2,"
  , "  shape_filter arg3,"
  , "  void *arg4"
  , ")"
  , "{"
  , "  return (shape_count_matching)(arg1, arg2, arg3, arg4);"
  , "}"
  , "void hs_bindgen_bafdac4e8add67f8 ("
  , "  struct shape *arg1,"
  , "  void (*arg2) ("
  , "  struct point *arg1,"
  , "  void *arg2"
  , "),"
  , "  void *arg3"
  , ")"
  , "{"
  , "  (shape_for_each_point)(arg1, arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_new@
foreign import ccall unsafe "hs_bindgen_eab5b423a48bd972" hs_bindgen_eab5b423a48bd972_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_new@
hs_bindgen_eab5b423a48bd972 ::
     BG.CInt
  -> IO (BG.Ptr Shape)
hs_bindgen_eab5b423a48bd972 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_eab5b423a48bd972_base (BG.toFFIType x0))

{-| __C declaration:__ @shape_new@

    __defined at:__ @..\/shape.h 16:15@

    __exported by:__ @follows_includes.h@
-}
shape_new ::
     BG.CInt
     -- ^ __C declaration:__ @count@
  -> IO (BG.Ptr Shape)
shape_new = hs_bindgen_eab5b423a48bd972

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_count_matching@
foreign import ccall unsafe "hs_bindgen_8ef0c1f8cefa7985" hs_bindgen_8ef0c1f8cefa7985_base ::
     BG.Ptr BG.Void
  -> BG.CInt
  -> BG.FunPtr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_count_matching@
hs_bindgen_8ef0c1f8cefa7985 ::
     BG.Ptr (BG.Ptr Shape)
  -> BG.CInt
  -> Shape_filter
  -> BG.Ptr BG.Void
  -> IO BG.CInt
hs_bindgen_8ef0c1f8cefa7985 =
  \x0 ->
    \x1 ->
      \x2 ->
        \x3 ->
          fmap BG.fromFFIType (hs_bindgen_8ef0c1f8cefa7985_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2) (BG.toFFIType x3))

{-| __C declaration:__ @shape_count_matching@

    __defined at:__ @..\/shape.h 17:5@

    __exported by:__ @follows_includes.h@
-}
shape_count_matching ::
     BG.Ptr (BG.Ptr Shape)
     -- ^ __C declaration:__ @shapes@
  -> BG.CInt
     -- ^ __C declaration:__ @count@
  -> Shape_filter
     -- ^ __C declaration:__ @filter@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @user@
  -> IO BG.CInt
shape_count_matching = hs_bindgen_8ef0c1f8cefa7985

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_for_each_point@
foreign import ccall unsafe "hs_bindgen_bafdac4e8add67f8" hs_bindgen_bafdac4e8add67f8_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarywalkthroughfollows_in_Geometry.Shape_Unsafe_shape_for_each_point@
hs_bindgen_bafdac4e8add67f8 ::
     BG.Ptr Shape
  -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_bafdac4e8add67f8 =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_bafdac4e8add67f8_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

{-| __C declaration:__ @shape_for_each_point@

    __defined at:__ @..\/shape.h 21:6@

    __exported by:__ @follows_includes.h@
-}
shape_for_each_point ::
     BG.Ptr Shape
     -- ^ __C declaration:__ @shape@
  -> BG.FunPtr (BG.Ptr Geometry.Point.Point -> BG.Ptr BG.Void -> IO ())
     -- ^ __C declaration:__ @visit@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @user@
  -> IO ()
shape_for_each_point = hs_bindgen_bafdac4e8add67f8
