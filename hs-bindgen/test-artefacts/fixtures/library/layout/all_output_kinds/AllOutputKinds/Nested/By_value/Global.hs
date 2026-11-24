{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module AllOutputKinds.Nested.By_value.Global
    ( AllOutputKinds.Nested.By_value.Global.by_value_zero
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import AllOutputKinds.Nested.By_value
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <all_output_kinds.h>"
  , "/* test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_zero */"
  , "__attribute__ ((const))"
  , "struct by_value const *hs_bindgen_fb55718c36c23f5d (void)"
  , "{"
  , "  return &by_value_zero;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_zero@
foreign import ccall unsafe "hs_bindgen_fb55718c36c23f5d" hs_bindgen_fb55718c36c23f5d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_get_by_value_zero@
hs_bindgen_fb55718c36c23f5d :: IO (PtrConst.PtrConst By_value)
hs_bindgen_fb55718c36c23f5d =
  fmap BG.fromFFIType hs_bindgen_fb55718c36c23f5d_base

{-# NOINLINE hs_bindgen_f141c8b46a376707 #-}
{-| __C declaration:__ @by_value_zero@

    __defined at:__ @all_output_kinds\/nested\/by_value.h 18:30@

    __exported by:__ @all_output_kinds.h@

    __unique:__ @test_librarylayoutall_output_kind_AllOutputKinds.Nested.By_value_by_value_zero@
-}
hs_bindgen_f141c8b46a376707 :: PtrConst.PtrConst By_value
hs_bindgen_f141c8b46a376707 =
  BG.unsafePerformIO hs_bindgen_fb55718c36c23f5d

{-# NOINLINE by_value_zero #-}
by_value_zero :: By_value
by_value_zero =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_f141c8b46a376707)
