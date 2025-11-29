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
  , "/* Example_get_\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_edgecasesadios_a27c24be75e20133 (void)"
  , "{"
  , "  return &\978\978;"
  , "}"
  , "/* Example_get_\978\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_edgecasesadios_1875f3779c35a7ab (void)"
  , "{"
  , "  return &\978\978\978;"
  , "}"
  ]))

{-| __unique:__ @Example_get_ϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_a27c24be75e20133" hs_bindgen_test_edgecasesadios_a27c24be75e20133 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h:21:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_a27c24be75e20133

{-| __unique:__ @Example_get_ϒϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_1875f3779c35a7ab" hs_bindgen_test_edgecasesadios_1875f3779c35a7ab ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h:24:18@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_1875f3779c35a7ab

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek cϒϒϒ_ptr)
