{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe._acos
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/ordinary_unnamed_decl_parent.h>"
  , "double hs_bindgen_dfefd17e87559d1a ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (_acos)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_unnamed_de_Example_Safe__acos@
foreign import ccall safe "hs_bindgen_dfefd17e87559d1a" hs_bindgen_dfefd17e87559d1a_base ::
     Double
  -> IO Double

-- __unique:__ @test_edgecasesordinary_unnamed_de_Example_Safe__acos@
hs_bindgen_dfefd17e87559d1a ::
     BG.CDouble
  -> IO BG.CDouble
hs_bindgen_dfefd17e87559d1a =
  BG.fromFFIType hs_bindgen_dfefd17e87559d1a_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_unnamed_decl_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_unnamed_decl_parent.h@
-}
_acos ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CDouble
_acos = hs_bindgen_dfefd17e87559d1a
