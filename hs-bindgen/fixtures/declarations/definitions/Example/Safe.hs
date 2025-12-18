{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_9cdc88a6d09442d6 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_9cdc88a6d09442d6" hs_bindgen_9cdc88a6d09442d6 ::
     FC.CDouble
  -> IO FC.CInt

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h:13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
foo = hs_bindgen_9cdc88a6d09442d6
