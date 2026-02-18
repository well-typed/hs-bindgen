{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "/* test_declarationsdeclarations_requ_Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0d4da37880af2263 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_declarationsdeclarations_requ_Example_get_f@
foreign import ccall unsafe "hs_bindgen_0d4da37880af2263" hs_bindgen_0d4da37880af2263_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_declarationsdeclarations_requ_Example_get_f@
hs_bindgen_0d4da37880af2263 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_0d4da37880af2263 =
  RIP.fromFFIType hs_bindgen_0d4da37880af2263_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f :: RIP.FunPtr (A -> IO ())
f = RIP.unsafePerformIO hs_bindgen_0d4da37880af2263
