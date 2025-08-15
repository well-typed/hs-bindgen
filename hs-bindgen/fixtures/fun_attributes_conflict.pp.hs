{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <fun_attributes_conflict.h>\nsigned int hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7 (signed int arg1) { return square_cp(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7 (signed int arg1) { return square_pc(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac (signed int arg1) { return square_cc(arg1); }\nsigned int hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3 (signed int arg1) { return square_pp(arg1); }\n")

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @square_cp(int)@
-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_10e6b4d386eec8f7" square_cp :: FC.CInt -> FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_d8e5dd6836af0ac7" square_pc :: FC.CInt -> FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_9a60da065e6486ac" square_cc :: FC.CInt -> FC.CInt

{-|

  Marked @__attribute((pure))__@

-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_conflict_1a2340fb8456aee3" square_pp :: FC.CInt -> IO FC.CInt
