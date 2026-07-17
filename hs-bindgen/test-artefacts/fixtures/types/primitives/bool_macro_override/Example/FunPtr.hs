{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    , Example.FunPtr.g
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/primitives/bool_macro_override.h>"
  , "/* test_typesprimitivesbool_macro_ov_Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aa1f6a0351ab44da (void)) ("
  , "  A arg1,"
  , "  _Bool arg2"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  , "/* test_typesprimitivesbool_macro_ov_Example_get_g */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c0944c7d91c77d18 (void)) ("
  , "  A arg1,"
  , "  bool arg2"
  , ")"
  , "{"
  , "  return &g;"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_f@
foreign import ccall unsafe "hs_bindgen_aa1f6a0351ab44da" hs_bindgen_aa1f6a0351ab44da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_f@
hs_bindgen_aa1f6a0351ab44da :: IO (BG.FunPtr (A -> BG.CBool -> IO ()))
hs_bindgen_aa1f6a0351ab44da =
  BG.fromFFIType hs_bindgen_aa1f6a0351ab44da_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_macro_override.h 10:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
f :: BG.FunPtr (A -> BG.CBool -> IO ())
f = BG.unsafePerformIO hs_bindgen_aa1f6a0351ab44da

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_g@
foreign import ccall unsafe "hs_bindgen_c0944c7d91c77d18" hs_bindgen_c0944c7d91c77d18_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_g@
hs_bindgen_c0944c7d91c77d18 :: IO (BG.FunPtr (A -> Bool' -> IO ()))
hs_bindgen_c0944c7d91c77d18 =
  BG.fromFFIType hs_bindgen_c0944c7d91c77d18_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @types\/primitives\/bool_macro_override.h 13:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
g :: BG.FunPtr (A -> Bool' -> IO ())
g = BG.unsafePerformIO hs_bindgen_c0944c7d91c77d18
