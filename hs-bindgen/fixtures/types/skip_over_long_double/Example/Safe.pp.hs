{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/skip_over_long_double.h>"
  , "void hs_bindgen_test_typesskip_over_long_double_fb32cb593bc1f7b8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  fun2(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @fun2@

    __defined at:__ @types\/skip_over_long_double.h:7:6@

    __exported by:__ @types\/skip_over_long_double.h@
-}
foreign import ccall safe "hs_bindgen_test_typesskip_over_long_double_fb32cb593bc1f7b8" fun2 ::
     FC.CInt
  -> IO ()
