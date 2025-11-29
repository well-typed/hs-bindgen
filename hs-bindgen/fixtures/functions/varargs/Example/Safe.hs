{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/varargs.h>"
  , "void hs_bindgen_test_functionsvarargs_2b5f87a3956903cc (void)"
  , "{"
  , "  h();"
  , "}"
  ]))

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h:8:6@

    __exported by:__ @functions\/varargs.h@

    __unique:__ @Example_Safe_h@
-}
foreign import ccall safe "hs_bindgen_test_functionsvarargs_2b5f87a3956903cc" h ::
     IO ()
