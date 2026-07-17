{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe._acos
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "double hs_bindgen_06a412f170b5ff91 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (_acos)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Safe__acos@
foreign import ccall safe "hs_bindgen_06a412f170b5ff91" hs_bindgen_06a412f170b5ff91_base ::
     Double
  -> IO Double

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Safe__acos@
hs_bindgen_06a412f170b5ff91 ::
     BG.CDouble
  -> IO BG.CDouble
hs_bindgen_06a412f170b5ff91 =
  BG.fromFFIType hs_bindgen_06a412f170b5ff91_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CDouble
_acos = hs_bindgen_06a412f170b5ff91
