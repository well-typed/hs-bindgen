{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Ops.Unsafe
    ( MyLib.Mylib.Ops.Unsafe.point_distance
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "double hs_bindgen_a4598320f89a2f76 ("
  , "  point_t *arg1,"
  , "  point_t *arg2"
  , ")"
  , "{"
  , "  return (point_distance)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib.Ops_Unsafe_point_distance@
foreign import ccall unsafe "hs_bindgen_a4598320f89a2f76" hs_bindgen_a4598320f89a2f76_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO BG.CDouble

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib.Ops_Unsafe_point_distance@
hs_bindgen_a4598320f89a2f76 ::
     BG.Ptr MyLib.Mylib.Types.Point_t
  -> BG.Ptr MyLib.Mylib.Types.Point_t
  -> IO BG.CDouble
hs_bindgen_a4598320f89a2f76 =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_a4598320f89a2f76_base (BG.toFFIType x0) (BG.toFFIType x1))

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
                                  hs_bindgen_a4598320f89a2f76 a2 b3))
