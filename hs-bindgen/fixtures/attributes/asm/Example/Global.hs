{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_attributesasm_Example_get_asm_labeled_variable@
hs_bindgen_e637e98af1313f88 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_e637e98af1313f88 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e637e98af1313f88_base

{-# NOINLINE asm_labeled_variable #-}
{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @attributes\/asm.h 2:12@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_variable :: Ptr.Ptr FC.CInt
asm_labeled_variable =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e637e98af1313f88
