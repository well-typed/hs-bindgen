{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/special/skip_over_long_double.h>"
  , "void hs_bindgen_test_typesspecialskip_over_long_d_fb32cb593bc1f7b8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  fun2(arg1);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_typesspecialskip_over_long_d_fb32cb593bc1f7b8" fun2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> IO ()
    )

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/special\/skip_over_long_double.h:7:6@

    __exported by:__ @types\/special\/skip_over_long_double.h@
-}
fun2 ::
     FC.CInt
  -> IO ()
fun2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun2_base
