{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/special/parse_failure_long_double.h>"
  , "void hs_bindgen_61793546aa44e36b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  fun2(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesspecialparse_failure_lo_Example_Unsafe_fun2@
foreign import ccall unsafe "hs_bindgen_61793546aa44e36b" hs_bindgen_61793546aa44e36b ::
     FC.CInt
  -> IO ()

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/parse_failure_long_double.h 7:6@

    __exported by:__ @types\/special\/parse_failure_long_double.h@
-}
fun2 ::
     FC.CInt
  -> IO ()
fun2 = hs_bindgen_61793546aa44e36b
