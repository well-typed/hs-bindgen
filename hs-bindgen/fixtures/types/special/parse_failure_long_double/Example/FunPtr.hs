{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "/* test_typesspecialparse_failure_lo_Example_get_fun2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d61a16f2d29260ed (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fun2;"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2@
foreign import ccall unsafe "hs_bindgen_d61a16f2d29260ed" hs_bindgen_d61a16f2d29260ed_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2@
hs_bindgen_d61a16f2d29260ed :: IO (Ptr.FunPtr (FC.CInt -> IO ()))
hs_bindgen_d61a16f2d29260ed =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d61a16f2d29260ed_base

{-# NOINLINE fun2 #-}
{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 :: Ptr.FunPtr (FC.CInt -> IO ())
fun2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d61a16f2d29260ed
