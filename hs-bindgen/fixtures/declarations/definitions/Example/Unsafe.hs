{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_test_declarationsdefinitions_84e4eb047d6694cd ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h:13:5@

    __exported by:__ @declarations\/definitions.h@

    __unique:__ @ExampleJust Unsafefoo@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsdefinitions_84e4eb047d6694cd" foo ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
