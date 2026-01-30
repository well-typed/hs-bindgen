{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Prelude (IO)

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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typesprimitivesbool_c23_Example_get_b@
hs_bindgen_31e1e443379b061b :: IO (Ptr.Ptr FC.CBool)
hs_bindgen_31e1e443379b061b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_31e1e443379b061b_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @types\/primitives\/bool_c23.h 3:13@

    __exported by:__ @types\/primitives\/bool_c23.h@
-}
b :: Ptr.Ptr FC.CBool
b =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31e1e443379b061b
