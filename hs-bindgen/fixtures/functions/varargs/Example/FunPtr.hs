{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/varargs.h>"
  , "/* get_h_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsvarargs_6344539fe0b25338 (void)) (void)"
  , "{"
  , "  return &h;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_h_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsvarargs_6344539fe0b25338" hs_bindgen_test_functionsvarargs_6344539fe0b25338 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE h_ptr #-}

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h:8:6@

    __exported by:__ @functions\/varargs.h@
-}
h_ptr :: Ptr.FunPtr (IO ())
h_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsvarargs_6344539fe0b25338
