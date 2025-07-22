{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"fun_attributes_conflict.h\"\nsigned int testmodule_square_cp (signed int arg1) { return square_cp(arg1); }\nsigned int testmodule_square_pc (signed int arg1) { return square_pc(arg1); }\nsigned int testmodule_square_cc (signed int arg1) { return square_cc(arg1); }\nsigned int testmodule_square_pp (signed int arg1) { return square_pp(arg1); }\n")

foreign import ccall safe "testmodule_square_cp" square_cp :: FC.CInt -> FC.CInt

foreign import ccall safe "testmodule_square_pc" square_pc :: FC.CInt -> FC.CInt

foreign import ccall safe "testmodule_square_cc" square_cc :: FC.CInt -> FC.CInt

-- C functions that have the @pure@ attribute may read from pointers, and since the contents of pointers can change, these functions are "impure" in the Haskell sense of the word, so we have to return the result in 'IO'. Note however that uses of a C-pure function can sometimes be safely encapsulated with @unsafePerformIO@ to obtain a Haskell-pure function.

foreign import ccall safe "testmodule_square_pp" square_pp :: FC.CInt -> IO FC.CInt
