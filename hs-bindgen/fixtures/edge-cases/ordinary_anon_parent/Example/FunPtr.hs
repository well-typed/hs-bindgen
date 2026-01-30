{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Prelude (IO)

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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_get__acos@
hs_bindgen_147bbeebcb063844 :: IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))
hs_bindgen_147bbeebcb063844 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_147bbeebcb063844_base

{-# NOINLINE _acos #-}
{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
_acos =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_147bbeebcb063844
