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
  [ "#include <types/scoping/deep_nesting.h>"
  , "/* test_typesscopingdeep_nesting_Example_get_X */"
  , "__attribute__ ((const))"
  , "struct baz *hs_bindgen_d6029a840689b228 (void)"
  , "{"
  , "  return &X;"
  , "}"
  ]))

-- __unique:__ @test_typesscopingdeep_nesting_Example_get_X@
foreign import ccall unsafe "hs_bindgen_d6029a840689b228" hs_bindgen_d6029a840689b228_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesscopingdeep_nesting_Example_get_X@
hs_bindgen_d6029a840689b228 :: IO (RIP.Ptr Baz)
hs_bindgen_d6029a840689b228 =
  RIP.fromFFIType hs_bindgen_d6029a840689b228_base

{-# NOINLINE x #-}
{-| __C declaration:__ @X@

    __defined at:__ @types\/scoping\/deep_nesting.h 25:19@

    __exported by:__ @types\/scoping\/deep_nesting.h@
-}
x :: RIP.Ptr Baz
x = RIP.unsafePerformIO hs_bindgen_d6029a840689b228
