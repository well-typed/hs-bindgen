{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/primitives/bool_c23.h>"
  , "/* test_typesprimitivesbool_c23_Example_get_b */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_31e1e443379b061b (void)"
  , "{"
  , "  return &b;"
  , "}"
  ]))

-- __unique:__ @test_typesprimitivesbool_c23_Example_get_b@
foreign import ccall unsafe "hs_bindgen_31e1e443379b061b" hs_bindgen_31e1e443379b061b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typesprimitivesbool_c23_Example_get_b@
hs_bindgen_31e1e443379b061b :: IO (BG.Ptr BG.CBool)
hs_bindgen_31e1e443379b061b =
  BG.fromFFIType hs_bindgen_31e1e443379b061b_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @types\/primitives\/bool_c23.h 3:13@

    __exported by:__ @types\/primitives\/bool_c23.h@
-}
b :: BG.Ptr BG.CBool
b = BG.unsafePerformIO hs_bindgen_31e1e443379b061b
