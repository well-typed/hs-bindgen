{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/primitives/bool_typedef_override.h>"
  , "/* test_typesprimitivesbool_typedef__Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_54e8e1ffb47ff670 (void)) ("
  , "  A arg1,"
  , "  bool arg2"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_typedef__Example_get_f@
foreign import ccall unsafe "hs_bindgen_54e8e1ffb47ff670" hs_bindgen_54e8e1ffb47ff670_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesprimitivesbool_typedef__Example_get_f@
hs_bindgen_54e8e1ffb47ff670 :: IO (RIP.FunPtr (A -> Bool' -> IO ()))
hs_bindgen_54e8e1ffb47ff670 =
  RIP.fromFFIType hs_bindgen_54e8e1ffb47ff670_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @types\/primitives\/bool_typedef_override.h 6:6@

    __exported by:__ @types\/primitives\/bool_typedef_override.h@
-}
f :: RIP.FunPtr (A -> Bool' -> IO ())
f = RIP.unsafePerformIO hs_bindgen_54e8e1ffb47ff670
