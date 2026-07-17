{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/field_name_reuse_omit.h>"
  , "/* test_declarationsfield_name_reuse__Example_get_foo */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8612ede7c1c9d541 (void)"
  , "{"
  , "  return &foo;"
  , "}"
  ]))

-- __unique:__ @test_declarationsfield_name_reuse__Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8612ede7c1c9d541" hs_bindgen_8612ede7c1c9d541_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_declarationsfield_name_reuse__Example_get_foo@
hs_bindgen_8612ede7c1c9d541 :: IO (BG.Ptr BG.CInt)
hs_bindgen_8612ede7c1c9d541 =
  BG.fromFFIType hs_bindgen_8612ede7c1c9d541_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/field_name_reuse_omit.h 23:12@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
foo :: BG.Ptr BG.CInt
foo = BG.unsafePerformIO hs_bindgen_8612ede7c1c9d541
