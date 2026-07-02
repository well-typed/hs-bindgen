{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.foo
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_declarationsfield_name_reuse__Example_get_foo@
hs_bindgen_8612ede7c1c9d541 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_8612ede7c1c9d541 =
  RIP.fromFFIType hs_bindgen_8612ede7c1c9d541_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/field_name_reuse_omit.h 23:12@

    __exported by:__ @declarations\/field_name_reuse_omit.h@
-}
foo :: RIP.Ptr RIP.CInt
foo = RIP.unsafePerformIO hs_bindgen_8612ede7c1c9d541
