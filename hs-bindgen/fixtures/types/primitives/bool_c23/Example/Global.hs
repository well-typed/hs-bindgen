{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typesprimitivesbool_c23_Example_get_b@
hs_bindgen_31e1e443379b061b :: IO (RIP.Ptr RIP.CBool)
hs_bindgen_31e1e443379b061b =
  RIP.fromFFIType hs_bindgen_31e1e443379b061b_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @types\/primitives\/bool_c23.h 3:13@

    __exported by:__ @types\/primitives\/bool_c23.h@
-}
b :: RIP.Ptr RIP.CBool
b = RIP.unsafePerformIO hs_bindgen_31e1e443379b061b
