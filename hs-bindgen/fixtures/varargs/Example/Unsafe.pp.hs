{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <varargs.h>"
  , "void hs_bindgen_test_varargs_ec9e8ecfd27bead3 (void)"
  , "{"
  , "  h();"
  , "}"
  ]))

{-| __C declaration:__ @h@

    __defined at:__ @varargs.h:8:6@

    __exported by:__ @varargs.h@
-}
foreign import ccall unsafe "hs_bindgen_test_varargs_ec9e8ecfd27bead3" h ::
     IO ()
