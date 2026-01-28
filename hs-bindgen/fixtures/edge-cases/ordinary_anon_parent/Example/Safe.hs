{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Prelude (Double, IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "double hs_bindgen_06a412f170b5ff91 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return _acos(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Safe__acos@
foreign import ccall safe "hs_bindgen_06a412f170b5ff91" hs_bindgen_06a412f170b5ff91_base ::
     Double
  -> IO Double

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Safe__acos@
hs_bindgen_06a412f170b5ff91 ::
     FC.CDouble
  -> IO FC.CDouble
hs_bindgen_06a412f170b5ff91 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_06a412f170b5ff91_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> IO FC.CDouble
_acos = hs_bindgen_06a412f170b5ff91
