{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    , Example.FunPtr.g
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_f@
hs_bindgen_aa1f6a0351ab44da :: IO (RIP.FunPtr (A -> RIP.CBool -> IO ()))
hs_bindgen_aa1f6a0351ab44da =
  RIP.fromFFIType hs_bindgen_aa1f6a0351ab44da_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_macro_override.h 10:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
f :: RIP.FunPtr (A -> RIP.CBool -> IO ())
f = RIP.unsafePerformIO hs_bindgen_aa1f6a0351ab44da

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_g@
foreign import ccall unsafe "hs_bindgen_c0944c7d91c77d18" hs_bindgen_c0944c7d91c77d18_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesprimitivesbool_macro_ov_Example_get_g@
hs_bindgen_c0944c7d91c77d18 :: IO (RIP.FunPtr (A -> Bool' -> IO ()))
hs_bindgen_c0944c7d91c77d18 =
  RIP.fromFFIType hs_bindgen_c0944c7d91c77d18_base

{-# NOINLINE g #-}
{-| __C declaration:__ @g@

    __defined at:__ @types\/primitives\/bool_macro_override.h 13:6@

    __exported by:__ @types\/primitives\/bool_macro_override.h@
-}
g :: RIP.FunPtr (A -> Bool' -> IO ())
g = RIP.unsafePerformIO hs_bindgen_c0944c7d91c77d18
