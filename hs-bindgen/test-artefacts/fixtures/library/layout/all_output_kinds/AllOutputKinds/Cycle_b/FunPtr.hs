{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Cycle_b.FunPtr
    ( AllOutputKinds.Cycle_b.FunPtr.cycle_b_new
    )
  where

import qualified AllOutputKinds.Cycle_a
import qualified AllOutputKinds.Nested.By_value
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Cycle_b
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Cycle_b_get_cycle_b_new */"
  , "__attribute__ ((const))"
  , "struct cycle_b *(*hs_bindgen_d2f4826a11286e8c (void)) ("
  , "  struct cycle_a *arg1,"
  , "  struct by_value arg2"
  , ")"
  , "{"
  , "  return &cycle_b_new;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_b_get_cycle_b_new@
foreign import ccall unsafe "hs_bindgen_d2f4826a11286e8c" hs_bindgen_d2f4826a11286e8c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_b_get_cycle_b_new@
hs_bindgen_d2f4826a11286e8c :: IO (BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_a.Cycle_a -> AllOutputKinds.Nested.By_value.By_value -> IO (BG.Ptr Cycle_b)))
hs_bindgen_d2f4826a11286e8c =
  fmap BG.fromFFIType hs_bindgen_d2f4826a11286e8c_base

{-# NOINLINE cycle_b_new #-}
{-| __C declaration:__ @cycle_b_new@

    __defined at:__ @all_output_kinds\/cycle_b.h 21:17@

    __exported by:__ @all_output_kinds.h@
-}
cycle_b_new :: BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_a.Cycle_a -> AllOutputKinds.Nested.By_value.By_value -> IO (BG.Ptr Cycle_b))
cycle_b_new =
  BG.unsafePerformIO hs_bindgen_d2f4826a11286e8c
