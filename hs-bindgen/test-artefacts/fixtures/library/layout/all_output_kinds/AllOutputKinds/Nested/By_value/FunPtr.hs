{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Nested.By_value.FunPtr
    ( AllOutputKinds.Nested.By_value.FunPtr.by_value_sum
    , AllOutputKinds.Nested.By_value.FunPtr.by_value_add
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Nested.By_value
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_sum */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_7370ae6f5937ed37 (void)) ("
  , "  struct by_value arg1"
  , ")"
  , "{"
  , "  return &by_value_sum;"
  , "}"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_add */"
  , "__attribute__ ((const))"
  , "struct by_value (*hs_bindgen_da5bb509db3fe32e (void)) ("
  , "  struct by_value arg1,"
  , "  struct by_value arg2"
  , ")"
  , "{"
  , "  return &by_value_add;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_sum@
foreign import ccall unsafe "hs_bindgen_7370ae6f5937ed37" hs_bindgen_7370ae6f5937ed37_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_sum@
hs_bindgen_7370ae6f5937ed37 :: IO (BG.FunPtr (By_value -> IO BG.CFloat))
hs_bindgen_7370ae6f5937ed37 =
  fmap BG.fromFFIType hs_bindgen_7370ae6f5937ed37_base

{-# NOINLINE by_value_sum #-}
{-| __C declaration:__ @by_value_sum@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 14:7@

    __exported by:__ @all_output_kinds.h@
-}
by_value_sum :: BG.FunPtr (By_value -> IO BG.CFloat)
by_value_sum =
  BG.unsafePerformIO hs_bindgen_7370ae6f5937ed37

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_add@
foreign import ccall unsafe "hs_bindgen_da5bb509db3fe32e" hs_bindgen_da5bb509db3fe32e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_add@
hs_bindgen_da5bb509db3fe32e :: IO (BG.FunPtr (By_value -> By_value -> IO By_value))
hs_bindgen_da5bb509db3fe32e =
  fmap BG.fromFFIType hs_bindgen_da5bb509db3fe32e_base

{-# NOINLINE by_value_add #-}
{-| __C declaration:__ @by_value_add@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 15:17@

    __exported by:__ @all_output_kinds.h@
-}
by_value_add :: BG.FunPtr (By_value -> By_value -> IO By_value)
by_value_add =
  BG.unsafePerformIO hs_bindgen_da5bb509db3fe32e
