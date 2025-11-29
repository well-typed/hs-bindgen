{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/adios.h>"
  , "/* test_edgecasesadios_Example_get_\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a5a7bbe6b8c53539 (void)"
  , "{"
  , "  return &\978\978;"
  , "}"
  , "/* test_edgecasesadios_Example_get_\978\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_31d6bde39787c8b8 (void)"
  , "{"
  , "  return &\978\978\978;"
  , "}"
  ]))

{-| __unique:__ @test_edgecasesadios_Example_get_ϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_a5a7bbe6b8c53539" hs_bindgen_a5a7bbe6b8c53539 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h:21:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a5a7bbe6b8c53539

{-| __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_31d6bde39787c8b8" hs_bindgen_31d6bde39787c8b8 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h:24:18@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31d6bde39787c8b8

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek cϒϒϒ_ptr)
