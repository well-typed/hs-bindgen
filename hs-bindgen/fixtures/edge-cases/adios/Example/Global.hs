{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
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

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a5a7bbe6b8c53539" hs_bindgen_a5a7bbe6b8c53539_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_edgecasesadios_Example_get_ϒϒ_ptr@
hs_bindgen_a5a7bbe6b8c53539 ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_a5a7bbe6b8c53539 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a5a7bbe6b8c53539_base

{-# NOINLINE cϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h:21:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a5a7bbe6b8c53539

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_31d6bde39787c8b8" hs_bindgen_31d6bde39787c8b8_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ_ptr@
hs_bindgen_31d6bde39787c8b8 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_31d6bde39787c8b8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_31d6bde39787c8b8_base

{-# NOINLINE cϒϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h:24:18@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒϒ_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31d6bde39787c8b8

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr cϒϒϒ_ptr))
