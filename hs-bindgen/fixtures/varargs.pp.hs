{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (unlines)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (Prelude.unlines
  [ "#include <varargs.h>"
  , "void hs_bindgen_test_varargs_a17c4f0272bbe42a (void)"
  , "{"
  , "  h();"
  , "}"
  , "/* get_h_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_varargs_6344539fe0b25338 (void)) (void)"
  , "{"
  , "  return &h;"
  , "}"
  ]))

{-| __C declaration:__ @h@

    __defined at:__ @varargs.h:8:6@

    __exported by:__ @varargs.h@
-}
foreign import ccall safe "hs_bindgen_test_varargs_a17c4f0272bbe42a" h ::
     IO ()

foreign import ccall unsafe "hs_bindgen_test_varargs_6344539fe0b25338" hs_bindgen_test_varargs_6344539fe0b25338 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE h_ptr #-}

{-| __C declaration:__ @h@

    __defined at:__ @varargs.h:8:6@

    __exported by:__ @varargs.h@
-}
h_ptr :: Ptr.FunPtr (IO ())
h_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_varargs_6344539fe0b25338
