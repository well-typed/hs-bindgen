{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.asm_labeled_variable
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <attributes/asm.h>"
  , "/* test_attributesasm_Example_get_asm_labeled_variable */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_e637e98af1313f88 (void)"
  , "{"
  , "  return &asm_labeled_variable;"
  , "}"
  ]))

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_variable@
foreign import ccall unsafe "hs_bindgen_e637e98af1313f88" hs_bindgen_e637e98af1313f88_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_variable@
hs_bindgen_e637e98af1313f88 :: IO (BG.Ptr BG.CInt)
hs_bindgen_e637e98af1313f88 =
  BG.fromFFIType hs_bindgen_e637e98af1313f88_base

{-# NOINLINE asm_labeled_variable #-}
{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @attributes\/asm.h 2:12@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_variable :: BG.Ptr BG.CInt
asm_labeled_variable =
  BG.unsafePerformIO hs_bindgen_e637e98af1313f88
