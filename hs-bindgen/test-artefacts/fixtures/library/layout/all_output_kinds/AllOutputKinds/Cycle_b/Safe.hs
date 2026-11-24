{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Cycle_b.Safe
    ( AllOutputKinds.Cycle_b.Safe.cycle_b_new
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
  , "struct cycle_b *hs_bindgen_291c405b00789d9c ("
  , "  struct cycle_a *arg1,"
  , "  struct by_value *arg2"
  , ")"
  , "{"
  , "  return (cycle_b_new)(arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_b_Safe_cycle_b_new@
foreign import ccall safe "hs_bindgen_291c405b00789d9c" hs_bindgen_291c405b00789d9c_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_b_Safe_cycle_b_new@
hs_bindgen_291c405b00789d9c ::
     BG.Ptr AllOutputKinds.Cycle_a.Cycle_a
  -> BG.Ptr AllOutputKinds.Nested.By_value.By_value
  -> IO (BG.Ptr Cycle_b)
hs_bindgen_291c405b00789d9c =
  \x0 ->
    \x1 ->
      fmap BG.fromFFIType (hs_bindgen_291c405b00789d9c_base (BG.toFFIType x0) (BG.toFFIType x1))

{-| __C declaration:__ @cycle_b_new@

    __defined at:__ @all_output_kinds\/cycle_b.h 21:17@

    __exported by:__ @all_output_kinds.h@
-}
cycle_b_new ::
     BG.Ptr AllOutputKinds.Cycle_a.Cycle_a
     -- ^ __C declaration:__ @owner@
  -> AllOutputKinds.Nested.By_value.By_value
     -- ^ __C declaration:__ @position@
  -> IO (BG.Ptr Cycle_b)
cycle_b_new =
  \owner0 ->
    \position1 ->
      BG.with position1 (\position2 ->
                           hs_bindgen_291c405b00789d9c owner0 position2)
