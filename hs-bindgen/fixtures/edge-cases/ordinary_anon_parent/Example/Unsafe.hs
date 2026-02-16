{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "double hs_bindgen_dca60678b5047ee4 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return _acos(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Unsafe__acos@
foreign import ccall unsafe "hs_bindgen_dca60678b5047ee4" hs_bindgen_dca60678b5047ee4_base ::
     Double
  -> IO Double

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Unsafe__acos@
hs_bindgen_dca60678b5047ee4 ::
     RIP.CDouble
  -> IO RIP.CDouble
hs_bindgen_dca60678b5047ee4 =
  RIP.fromFFIType hs_bindgen_dca60678b5047ee4_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO RIP.CDouble
_acos = hs_bindgen_dca60678b5047ee4
