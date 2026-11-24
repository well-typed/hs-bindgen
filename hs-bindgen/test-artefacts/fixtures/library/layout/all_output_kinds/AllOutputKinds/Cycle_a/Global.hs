{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Cycle_a.Global
    ( AllOutputKinds.Cycle_a.Global.cycle_a_total
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_total */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c0d025eb8dce1dac (void)"
  , "{"
  , "  return &cycle_a_total;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_total@
foreign import ccall unsafe "hs_bindgen_c0d025eb8dce1dac" hs_bindgen_c0d025eb8dce1dac_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Cycle_a_get_cycle_a_total@
hs_bindgen_c0d025eb8dce1dac :: IO (BG.Ptr BG.CInt)
hs_bindgen_c0d025eb8dce1dac =
  fmap BG.fromFFIType hs_bindgen_c0d025eb8dce1dac_base

{-# NOINLINE cycle_a_total #-}
{-| __C declaration:__ @cycle_a_total@

    __defined at:__ @all_output_kinds\/cycle_a.h 25:12@

    __exported by:__ @all_output_kinds.h@
-}
cycle_a_total :: BG.Ptr BG.CInt
cycle_a_total =
  BG.unsafePerformIO hs_bindgen_c0d025eb8dce1dac
