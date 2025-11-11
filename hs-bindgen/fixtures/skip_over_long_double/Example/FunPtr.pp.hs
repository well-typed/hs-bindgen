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
  [ "#include <skip_over_long_double.h>"
  , "/* get_fun2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fun2;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa" hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa ::
     IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-# NOINLINE fun2_ptr #-}

{-| __C declaration:__ @fun2@

    __defined at:__ @skip_over_long_double.h:7:6@

    __exported by:__ @skip_over_long_double.h@
-}
fun2_ptr :: Ptr.FunPtr (FC.CInt -> IO ())
fun2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_skip_over_long_double_d1bf59c1516f6bfa
