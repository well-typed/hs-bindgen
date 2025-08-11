{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <fun_attributes_conflict.h>\nsigned int test_internal_square_cp (signed int arg1) { return square_cp(arg1); }\nsigned int test_internal_square_pc (signed int arg1) { return square_pc(arg1); }\nsigned int test_internal_square_cc (signed int arg1) { return square_cc(arg1); }\nsigned int test_internal_square_pp (signed int arg1) { return square_pp(arg1); }\n")

{-| Conflicting attributes on functions for llvm/clang versions 18 and up

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @square_cp(int)@
-}
foreign import ccall safe "test_internal_square_cp" square_cp :: FC.CInt -> FC.CInt

foreign import ccall safe "test_internal_square_pc" square_pc :: FC.CInt -> FC.CInt

foreign import ccall safe "test_internal_square_cc" square_cc :: FC.CInt -> FC.CInt

{-|

  Marked @__attribute((pure))__@

-}
foreign import ccall safe "test_internal_square_pp" square_pp :: FC.CInt -> IO FC.CInt
