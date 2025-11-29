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
  , "/* get_\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_edgecasesadios_e4b974661ff038a0 (void)"
  , "{"
  , "  return &\978\978;"
  , "}"
  , "/* get_\978\978\978_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_edgecasesadios_c538a25ba7055dd4 (void)"
  , "{"
  , "  return &\978\978\978;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_ϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_e4b974661ff038a0" hs_bindgen_test_edgecasesadios_e4b974661ff038a0 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h:21:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_e4b974661ff038a0

{-| __unique:__ @ExampleNothingget_ϒϒϒ_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesadios_c538a25ba7055dd4" hs_bindgen_test_edgecasesadios_c538a25ba7055dd4 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h:24:18@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesadios_c538a25ba7055dd4

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek cϒϒϒ_ptr)
