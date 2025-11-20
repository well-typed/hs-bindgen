{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/varargs.h>"
  , "void hs_bindgen_test_functionsvarargs_ec9e8ecfd27bead3 (void)"
  , "{"
  , "  h();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsvarargs_ec9e8ecfd27bead3" h_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO ()
    )

{-| __C declaration:__ @h@

    __defined at:__ @functions\/varargs.h:8:6@

    __exported by:__ @functions\/varargs.h@
-}
h ::
     IO ()
h =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType h_base
