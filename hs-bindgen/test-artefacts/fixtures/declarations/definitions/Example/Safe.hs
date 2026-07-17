{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_9cdc88a6d09442d6 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_9cdc88a6d09442d6" hs_bindgen_9cdc88a6d09442d6_base ::
     Double
  -> IO BG.Int32

-- __unique:__ @test_declarationsdefinitions_Example_Safe_foo@
hs_bindgen_9cdc88a6d09442d6 ::
     BG.CDouble
  -> IO BG.CInt
hs_bindgen_9cdc88a6d09442d6 =
  BG.fromFFIType hs_bindgen_9cdc88a6d09442d6_base

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
foo = hs_bindgen_9cdc88a6d09442d6
