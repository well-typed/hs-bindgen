{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "/* test_edgecasesordinary_anon_paren_Example_get__acos */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_147bbeebcb063844 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &_acos;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_get__acos@
foreign import ccall unsafe "hs_bindgen_147bbeebcb063844" hs_bindgen_147bbeebcb063844_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_get__acos@
hs_bindgen_147bbeebcb063844 :: IO (RIP.FunPtr (RIP.CDouble -> IO RIP.CDouble))
hs_bindgen_147bbeebcb063844 =
  RIP.fromFFIType hs_bindgen_147bbeebcb063844_base

{-# NOINLINE _acos #-}
{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos :: RIP.FunPtr (RIP.CDouble -> IO RIP.CDouble)
_acos =
  RIP.unsafePerformIO hs_bindgen_147bbeebcb063844
