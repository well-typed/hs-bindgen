{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     RIP.CDouble
  -> IO RIP.CDouble
hs_bindgen_06a412f170b5ff91 =
  RIP.fromFFIType hs_bindgen_06a412f170b5ff91_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO RIP.CDouble
_acos = hs_bindgen_06a412f170b5ff91
