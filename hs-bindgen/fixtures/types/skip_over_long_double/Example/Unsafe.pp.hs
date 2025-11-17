{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/skip_over_long_double.h>"
  , "void hs_bindgen_test_typesskip_over_long_double_5ebf8088e71802cc ("
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
foreign import ccall unsafe "hs_bindgen_test_typesskip_over_long_double_5ebf8088e71802cc" fun2 ::
     FC.CInt
  -> IO ()
