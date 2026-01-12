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
  , "/* test_functionsvarargs_Example_get_h */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d7b5ad93f4d7fa04 (void)) (void)"
  , "{"
  , "  return &h;"
  , "}"
  ]))

-- __unique:__ @test_functionsvarargs_Example_get_h@
foreign import ccall unsafe "hs_bindgen_d7b5ad93f4d7fa04" hs_bindgen_d7b5ad93f4d7fa04 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE h #-}
{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h 8:6@

    __exported by:__ @functions\/varargs.h@
-}
h :: Ptr.FunPtr (IO ())
h =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d7b5ad93f4d7fa04
