{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/trans_dep/macro_trans_dep_missing.h>"
  , "/* test_bindingspecstrans_depmacro__Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a003101b3eb21238 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecstrans_depmacro__Example_get_foo@
foreign import ccall unsafe "hs_bindgen_a003101b3eb21238" hs_bindgen_a003101b3eb21238_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecstrans_depmacro__Example_get_foo@
hs_bindgen_a003101b3eb21238 :: IO (RIP.FunPtr (M.B -> IO ()))
hs_bindgen_a003101b3eb21238 =
  RIP.fromFFIType hs_bindgen_a003101b3eb21238_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h 7:6@

    __exported by:__ @binding-specs\/trans_dep\/macro_trans_dep_missing.h@
-}
foo :: RIP.FunPtr (M.B -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_a003101b3eb21238
