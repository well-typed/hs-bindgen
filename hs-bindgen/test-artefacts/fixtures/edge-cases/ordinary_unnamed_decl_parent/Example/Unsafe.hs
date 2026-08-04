{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe._acos
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <edge-cases/ordinary_unnamed_decl_parent.h>"
  , "double hs_bindgen_751dc472004ee482 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (_acos)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_unnamed_de_Example_Unsafe__acos@
foreign import ccall unsafe "hs_bindgen_751dc472004ee482" hs_bindgen_751dc472004ee482_base ::
     Double
  -> IO Double

-- __unique:__ @test_edgecasesordinary_unnamed_de_Example_Unsafe__acos@
hs_bindgen_751dc472004ee482 ::
     BG.CDouble
  -> IO BG.CDouble
hs_bindgen_751dc472004ee482 =
  BG.fromFFIType hs_bindgen_751dc472004ee482_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_unnamed_decl_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_unnamed_decl_parent.h@
-}
_acos ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CDouble
_acos = hs_bindgen_751dc472004ee482
