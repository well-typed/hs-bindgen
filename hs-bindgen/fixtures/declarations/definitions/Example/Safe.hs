{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Prelude (Double, IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_9cdc88a6d09442d6 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_9cdc88a6d09442d6" hs_bindgen_9cdc88a6d09442d6_base ::
     Double
  -> IO GHC.Int.Int32

-- __unique:__ @test_declarationsdefinitions_Example_Safe_foo@
hs_bindgen_9cdc88a6d09442d6 ::
     FC.CDouble
  -> IO FC.CInt
hs_bindgen_9cdc88a6d09442d6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_9cdc88a6d09442d6_base

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
foo = hs_bindgen_9cdc88a6d09442d6
