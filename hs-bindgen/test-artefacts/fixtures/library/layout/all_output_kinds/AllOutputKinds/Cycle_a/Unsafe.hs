{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Cycle_a.Unsafe
    ( AllOutputKinds.Cycle_a.Unsafe.cycle_a_new
    , AllOutputKinds.Cycle_a.Unsafe.cycle_a_on_pair
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
  , "struct cycle_a *hs_bindgen_b5a33bdcc33da912 ("
  , "  struct by_value *arg1"
  , ")"
  , "{"
  , "  return (cycle_a_new)(*arg1);"
  , "}"
  , "void hs_bindgen_aad17c09be80bc16 ("
  , "  struct cycle_a *arg1,"
  , "  void (*arg2) ("
  , "  struct cycle_b *arg1,"
  , "  struct cycle_b *arg2"
  , ")"
  , ")"
  , "{"
  , "  (cycle_a_on_pair)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_Unsafe_cycle_a_new@
foreign import ccall unsafe "hs_bindgen_b5a33bdcc33da912" hs_bindgen_b5a33bdcc33da912_base ::
     BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_Unsafe_cycle_a_new@
hs_bindgen_b5a33bdcc33da912 ::
     BG.Ptr AllOutputKinds.Nested.By_value.By_value
  -> IO (BG.Ptr Cycle_a)
hs_bindgen_b5a33bdcc33da912 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_b5a33bdcc33da912_base (BG.toFFIType x0))

{-| __C declaration:__ @cycle_a_new@

    __defined at:__ @all_output_kinds\/cycle_a.h 19:17@

    __exported by:__ @all_output_kinds.h@
-}
cycle_a_new ::
     AllOutputKinds.Nested.By_value.By_value
     -- ^ __C declaration:__ @position@
  -> IO (BG.Ptr Cycle_a)
cycle_a_new =
  \position0 ->
    BG.with position0 (\position1 ->
                         hs_bindgen_b5a33bdcc33da912 position1)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_Unsafe_cycle_a_on_pair@
foreign import ccall unsafe "hs_bindgen_aad17c09be80bc16" hs_bindgen_aad17c09be80bc16_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_Unsafe_cycle_a_on_pair@
hs_bindgen_aad17c09be80bc16 ::
     BG.Ptr Cycle_a
  -> BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ())
  -> IO ()
hs_bindgen_aad17c09be80bc16 =
  \x0 ->
    \x1 ->
      hs_bindgen_aad17c09be80bc16_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @cycle_a_on_pair@

    __defined at:__ @all_output_kinds\/cycle_a.h 23:6@

    __exported by:__ @all_output_kinds.h@
-}
cycle_a_on_pair ::
     BG.Ptr Cycle_a
     -- ^ __C declaration:__ @a@
  -> BG.FunPtr (BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> BG.Ptr AllOutputKinds.Cycle_b.Cycle_b -> IO ())
     -- ^ __C declaration:__ @raw_callback@
  -> IO ()
cycle_a_on_pair = hs_bindgen_aad17c09be80bc16
