{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <attributes/asm.h>"
  , "/* ExampleNothingget_asm_labeled_function_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_attributesasm_b6d695e6a1f2622e (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return &asm_labeled_function;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_asm_labeled_function_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_attributesasm_b6d695e6a1f2622e" hs_bindgen_test_attributesasm_b6d695e6a1f2622e ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE asm_labeled_function_ptr #-}

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @attributes\/asm.h:4:5@

    __exported by:__ @attributes\/asm.h@
-}
asm_labeled_function_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
asm_labeled_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_attributesasm_b6d695e6a1f2622e
