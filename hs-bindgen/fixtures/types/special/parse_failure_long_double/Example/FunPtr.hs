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
  [ "#include <types/special/parse_failure_long_double.h>"
  , "/* test_typesspecialparse_failure_lo_Example_get_fun2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c526f22b76547216 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &fun2;"
  , "}"
  ]))

-- | __unique:__ @test_typesspecialparse_failure_lo_Example_get_fun2_ptr@
foreign import ccall unsafe "hs_bindgen_c526f22b76547216" hs_bindgen_c526f22b76547216 ::
     IO (Ptr.FunPtr (FC.CInt -> IO ()))

{-# NOINLINE fun2_ptr #-}

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h:7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2_ptr :: Ptr.FunPtr (FC.CInt -> IO ())
fun2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c526f22b76547216
