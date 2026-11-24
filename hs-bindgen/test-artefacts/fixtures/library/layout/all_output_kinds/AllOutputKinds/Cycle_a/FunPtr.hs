{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Cycle_a.FunPtr
    ( AllOutputKinds.Cycle_a.FunPtr.cycle_a_new
    , AllOutputKinds.Cycle_a.FunPtr.cycle_a_on_pair
    )
  where

import qualified AllOutputKinds.Cycle_b
import qualified AllOutputKinds.Nested.By_value
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Cycle_a
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_new */"
  , "__attribute__ ((const))"
  , "struct cycle_a *(*hs_bindgen_4301b398f7b9a666 (void)) ("
  , "  struct by_value arg1"
  , ")"
  , "{"
  , "  return &cycle_a_new;"
  , "}"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_on_pair */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_76aacf1bad1ca03a (void)) ("
  , "  struct cycle_a *arg1,"
  , "  void (*arg2) ("
  , "  struct cycle_b *arg1,"
  , "  struct cycle_b *arg2"
  , ")"
  , ")"
  , "{"
  , "  return &cycle_a_on_pair;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_new@
foreign import ccall unsafe "hs_bindgen_4301b398f7b9a666" hs_bindgen_4301b398f7b9a666_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_new@
hs_bindgen_4301b398f7b9a666 :: IO (BG.FunPtr (AllOutputKinds.Nested.By_value.By_value -> IO (BG.Ptr Cycle_a)))
hs_bindgen_4301b398f7b9a666 =
  fmap BG.fromFFIType hs_bindgen_4301b398f7b9a666_base

{-# NOINLINE cycle_a_new #-}
{-| __C declaration:__ @cycle_a_new@

    __defined at:__ @all_output_kinds\/cycle_a.h 19:17@

    __exported by:__ @all_output_kinds.h@
-}
cycle_a_new :: BG.FunPtr (AllOutputKinds.Nested.By_value.By_value -> IO (BG.Ptr Cycle_a))
cycle_a_new =
  BG.unsafePerformIO hs_bindgen_4301b398f7b9a666

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_on_pair@
foreign import ccall unsafe "hs_bindgen_76aacf1bad1ca03a" hs_bindgen_76aacf1bad1ca03a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_on_pair@
hs_bindgen_76aacf1bad1ca03a :: IO (BG.FunPtr (BG.Ptr Cycle_a -> BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()) -> IO ()))
hs_bindgen_76aacf1bad1ca03a =
  fmap BG.fromFFIType hs_bindgen_76aacf1bad1ca03a_base

{-# NOINLINE cycle_a_on_pair #-}
{-| __C declaration:__ @cycle_a_on_pair@

    __defined at:__ @all_output_kinds\/cycle_a.h 23:6@

    __exported by:__ @all_output_kinds.h@
-}
cycle_a_on_pair :: BG.FunPtr (BG.Ptr Cycle_a -> BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ()) -> IO ())
cycle_a_on_pair =
  BG.unsafePerformIO hs_bindgen_76aacf1bad1ca03a
