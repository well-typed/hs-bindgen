{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.x
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/scoping/nesting.h>"
  , "/* test_typesscopingnesting_Example_get_X */"
  , "__attribute__ ((const))"
  , "struct bar *hs_bindgen_7640032abd722ca9 (void)"
  , "{"
  , "  return &X;"
  , "}"
  ]))

-- __unique:__ @test_typesscopingnesting_Example_get_X@
foreign import ccall unsafe "hs_bindgen_7640032abd722ca9" hs_bindgen_7640032abd722ca9_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesscopingnesting_Example_get_X@
hs_bindgen_7640032abd722ca9 :: IO (BG.Ptr Bar)
hs_bindgen_7640032abd722ca9 =
  BG.fromFFIType hs_bindgen_7640032abd722ca9_base

{-# NOINLINE x #-}
{-| __C declaration:__ @X@

    __defined at:__ @types\/scoping\/nesting.h 26:19@

    __exported by:__ @types\/scoping\/nesting.h@
-}
x :: BG.Ptr Bar
x = BG.unsafePerformIO hs_bindgen_7640032abd722ca9
