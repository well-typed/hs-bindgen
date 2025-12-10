{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/primitives/bool_c23.h>"
  , "/* test_typesprimitivesbool_c23_Example_get_b_ptr */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_1e3421d11afdb5be (void)"
  , "{"
  , "  return &b;"
  , "}"
  ]))

-- | __unique:__ @test_typesprimitivesbool_c23_Example_get_b_ptr@
foreign import ccall unsafe "hs_bindgen_1e3421d11afdb5be" hs_bindgen_1e3421d11afdb5be ::
     IO (Ptr.Ptr FC.CBool)

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/primitives\/bool_c23.h:3:13@

    __exported by:__ @types\/primitives\/bool_c23.h@
-}
b_ptr :: Ptr.Ptr FC.CBool
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1e3421d11afdb5be
