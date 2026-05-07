{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.x
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/scoping/wide_nesting.h>"
  , "/* test_typesscopingwide_nesting_Example_get_X */"
  , "__attribute__ ((const))"
  , "struct baz *hs_bindgen_a295757aed8ccce9 (void)"
  , "{"
  , "  return &X;"
  , "}"
  ]))

-- __unique:__ @test_typesscopingwide_nesting_Example_get_X@
foreign import ccall unsafe "hs_bindgen_a295757aed8ccce9" hs_bindgen_a295757aed8ccce9_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesscopingwide_nesting_Example_get_X@
hs_bindgen_a295757aed8ccce9 :: IO (RIP.Ptr Baz)
hs_bindgen_a295757aed8ccce9 =
  RIP.fromFFIType hs_bindgen_a295757aed8ccce9_base

{-# NOINLINE x #-}
{-| __C declaration:__ @X@

    __defined at:__ @types\/scoping\/wide_nesting.h 26:19@

    __exported by:__ @types\/scoping\/wide_nesting.h@
-}
x :: RIP.Ptr Baz
x = RIP.unsafePerformIO hs_bindgen_a295757aed8ccce9
