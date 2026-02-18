{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_variable@
hs_bindgen_e637e98af1313f88 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_e637e98af1313f88 =
  RIP.fromFFIType hs_bindgen_e637e98af1313f88_base

{-# NOINLINE asm_labeled_variable #-}
{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @attributes\/asm.h 2:12@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_variable :: RIP.Ptr RIP.CInt
asm_labeled_variable =
  RIP.unsafePerformIO hs_bindgen_e637e98af1313f88
