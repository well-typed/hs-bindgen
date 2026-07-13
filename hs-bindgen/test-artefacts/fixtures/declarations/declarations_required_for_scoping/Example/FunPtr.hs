{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_declarationsdeclarations_requ_Example_get_f@
hs_bindgen_0d4da37880af2263 :: IO (BG.FunPtr (A -> IO ()))
hs_bindgen_0d4da37880af2263 =
  BG.fromFFIType hs_bindgen_0d4da37880af2263_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f :: BG.FunPtr (A -> IO ())
f = BG.unsafePerformIO hs_bindgen_0d4da37880af2263
