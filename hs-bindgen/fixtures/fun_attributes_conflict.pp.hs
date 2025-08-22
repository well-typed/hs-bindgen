{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-foreign-imports #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <fun_attributes_conflict.h>\nsigned int hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7 (signed int arg1) { return square_cp(arg1); }\n/* get_square_cp_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_b0be55d765c54fd2 (void)) (signed int arg1) { return &square_cp; } \nsigned int hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7 (signed int arg1) { return square_pc(arg1); }\n/* get_square_pc_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_bed19d597ecaa453 (void)) (signed int arg1) { return &square_pc; } \nsigned int hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac (signed int arg1) { return square_cc(arg1); }\n/* get_square_cc_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_744a10838ba7c4c7 (void)) (signed int arg1) { return &square_cc; } \nsigned int hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3 (signed int arg1) { return square_pp(arg1); }\n/* get_square_pp_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_fun_attributes_conflict_6d005de2b144cc17 (void)) (signed int arg1) { return &square_pp; } \n")

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @square_cp(int)@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7" square_cp
  :: FC.CInt
     {- ^ __from C:__ @x@ -}
  -> FC.CInt

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @square_cp(int)@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_b0be55d765c54fd2" square_cp_ptr
  :: F.FunPtr (FC.CInt -> IO FC.CInt)

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7" square_pc
  :: FC.CInt
     {- ^ __from C:__ @x@ -}
  -> FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_bed19d597ecaa453" square_pc_ptr
  :: F.FunPtr (FC.CInt -> IO FC.CInt)

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac" square_cc
  :: FC.CInt
     {- ^ __from C:__ @x@ -}
  -> FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_744a10838ba7c4c7" square_cc_ptr
  :: F.FunPtr (FC.CInt -> IO FC.CInt)

{-|

  Marked @__attribute((pure))__@

-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3" square_pp
  :: FC.CInt
     {- ^ __from C:__ @x@ -}
  -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_6d005de2b144cc17" square_pp_ptr
  :: F.FunPtr (FC.CInt -> IO FC.CInt)
