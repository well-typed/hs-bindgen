{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <asm.h>\nsigned int hs_bindgen_test_asm_b15fc0d2b3d7c9a1 (signed int arg1, signed int arg2) { return asm_labeled_function(arg1, arg2); }\n/* get_asm_labeled_function_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_asm_b6d695e6a1f2622e (void)) (signed int arg1, signed int arg2) { return &asm_labeled_function; } \n/* get_asm_labeled_variable_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_asm_f13c50d1f1661525 (void) { return &asm_labeled_variable; } \n")

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @asm.h:4:5@

    __exported by:__ @asm.h@
-}
foreign import ccall safe "hs_bindgen_test_asm_b15fc0d2b3d7c9a1" asm_labeled_function ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CInt

foreign import ccall unsafe "hs_bindgen_test_asm_b6d695e6a1f2622e" hs_bindgen_test_asm_b6d695e6a1f2622e ::
     IO (Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt))

{-# NOINLINE asm_labeled_function_ptr #-}

{-| __C declaration:__ @asm_labeled_function@

    __defined at:__ @asm.h:4:5@

    __exported by:__ @asm.h@
-}
asm_labeled_function_ptr :: Ptr.FunPtr (FC.CInt -> FC.CInt -> IO FC.CInt)
asm_labeled_function_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_asm_b6d695e6a1f2622e

foreign import ccall unsafe "hs_bindgen_test_asm_f13c50d1f1661525" hs_bindgen_test_asm_f13c50d1f1661525 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE asm_labeled_variable_ptr #-}

{-| __C declaration:__ @asm_labeled_variable@

    __defined at:__ @asm.h:2:12@

    __exported by:__ @asm.h@
-}
asm_labeled_variable_ptr :: Ptr.Ptr FC.CInt
asm_labeled_variable_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_asm_f13c50d1f1661525
