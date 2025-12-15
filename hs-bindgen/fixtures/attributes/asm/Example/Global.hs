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
  , "/* test_attributesasm_Example_get_asm_labeled_variable_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_f26ea231d0d58288 (void)"
  , "{"
  , "  return &asm_labeled_variable;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f26ea231d0d58288" hs_bindgen_f26ea231d0d58288_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_attributesasm_Example_get_asm_labeled_variable_ptr@
hs_bindgen_f26ea231d0d58288 ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_f26ea231d0d58288 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f26ea231d0d58288_base

{-# NOINLINE asm_labeled_variable_ptr #-}

{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @attributes\/asm.h:2:12@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_variable_ptr :: Ptr.Ptr FC.CInt
asm_labeled_variable_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f26ea231d0d58288
