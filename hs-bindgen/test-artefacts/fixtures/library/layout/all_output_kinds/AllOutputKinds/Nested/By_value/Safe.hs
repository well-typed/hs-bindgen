{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Nested.By_value.Safe
    ( AllOutputKinds.Nested.By_value.Safe.by_value_sum
    , AllOutputKinds.Nested.By_value.Safe.by_value_add
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Nested.By_value
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "float hs_bindgen_2b83ab18e5d1bef2 ("
  , "  struct by_value *arg1"
  , ")"
  , "{"
  , "  return (by_value_sum)(*arg1);"
  , "}"
  , "void hs_bindgen_d29fb230e5d09071 ("
  , "  struct by_value *arg1,"
  , "  struct by_value *arg2,"
  , "  struct by_value *arg3"
  , ")"
  , "{"
  , "  *arg3 = (by_value_add)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_Safe_by_value_sum@
foreign import ccall safe "hs_bindgen_2b83ab18e5d1bef2" hs_bindgen_2b83ab18e5d1bef2_base ::
     BG.Ptr BG.Void
  -> IO BG.CFloat

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_Safe_by_value_sum@
hs_bindgen_2b83ab18e5d1bef2 ::
     BG.Ptr By_value
  -> IO BG.CFloat
hs_bindgen_2b83ab18e5d1bef2 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2b83ab18e5d1bef2_base (BG.toFFIType x0))

{-| __C declaration:__ @by_value_sum@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 14:7@

    __exported by:__ @all_output_kinds.h@
-}
by_value_sum ::
     By_value
     -- ^ __C declaration:__ @v@
  -> IO BG.CFloat
by_value_sum =
  \v0 ->
    BG.with v0 (\v1 -> hs_bindgen_2b83ab18e5d1bef2 v1)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_Safe_by_value_add@
foreign import ccall safe "hs_bindgen_d29fb230e5d09071" hs_bindgen_d29fb230e5d09071_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_Safe_by_value_add@
hs_bindgen_d29fb230e5d09071 ::
     BG.Ptr By_value
  -> BG.Ptr By_value
  -> BG.Ptr By_value
  -> IO ()
hs_bindgen_d29fb230e5d09071 =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_d29fb230e5d09071_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

{-| __C declaration:__ @by_value_add@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 15:17@

    __exported by:__ @all_output_kinds.h@
-}
by_value_add ::
     By_value
     -- ^ __C declaration:__ @a@
  -> By_value
     -- ^ __C declaration:__ @b@
  -> IO By_value
by_value_add =
  \a0 ->
    \b1 ->
      BG.with a0 (\a2 ->
                    BG.with b1 (\b3 ->
                                  BG.allocaAndPeek (\res4 ->
                                                      hs_bindgen_d29fb230e5d09071 a2 b3 res4)))
