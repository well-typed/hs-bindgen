{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
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

{-| __unique:__ @test_attributesasm_Example_get_asm_labeled_variable_ptr@
-}
foreign import ccall unsafe "hs_bindgen_f26ea231d0d58288" hs_bindgen_f26ea231d0d58288 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE asm_labeled_variable_ptr #-}

{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @attributes\/asm.h:2:12@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_variable_ptr :: Ptr.Ptr FC.CInt
asm_labeled_variable_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f26ea231d0d58288
