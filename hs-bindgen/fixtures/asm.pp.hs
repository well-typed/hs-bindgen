{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <asm.h>\n/* get_asm_labeled_variable_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_asm_d2d42e5b0c00988a (void) { return &asm_labeled_variable; } \nsigned int hs_bindgen_test_asm_54c5278e738a284f (signed int arg1, signed int arg2) { return asm_labeled_function(arg1, arg2); }\n")

foreign import ccall safe "hs_bindgen_test_asm_d2d42e5b0c00988a" asm_labeled_variable_ptr
  :: F.Ptr FC.CInt

{-| __from C:__ @asm_labeled_function@ -}
foreign import ccall safe "hs_bindgen_test_asm_54c5278e738a284f" asm_labeled_function
  :: FC.CInt
     {- ^ __from C:__ @x@ -}
  -> FC.CInt
     {- ^ __from C:__ @y@ -}
  -> IO FC.CInt
