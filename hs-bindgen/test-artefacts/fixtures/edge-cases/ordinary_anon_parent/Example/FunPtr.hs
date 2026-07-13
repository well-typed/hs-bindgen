{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr._acos
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_get__acos@
hs_bindgen_147bbeebcb063844 :: IO (BG.FunPtr (BG.CDouble -> IO BG.CDouble))
hs_bindgen_147bbeebcb063844 =
  BG.fromFFIType hs_bindgen_147bbeebcb063844_base

{-# NOINLINE _acos #-}
{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos :: BG.FunPtr (BG.CDouble -> IO BG.CDouble)
_acos =
  BG.unsafePerformIO hs_bindgen_147bbeebcb063844
