{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/adios.h>"
  , "/* test_edgecasesadios_Example_get_\978\978 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_aa137b95cfa81f42 (void)"
  , "{"
  , "  return &\978\978;"
  , "}"
  , "/* test_edgecasesadios_Example_get_\978\978\978 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_7e61df7271c4ff58 (void)"
  , "{"
  , "  return &\978\978\978;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒ@
foreign import ccall unsafe "hs_bindgen_aa137b95cfa81f42" hs_bindgen_aa137b95cfa81f42_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒ@
hs_bindgen_aa137b95cfa81f42 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_aa137b95cfa81f42 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_aa137b95cfa81f42_base

{-# NOINLINE cϒϒ #-}
{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h 26:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ :: Ptr.Ptr FC.CInt
cϒϒ =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa137b95cfa81f42

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
foreign import ccall unsafe "hs_bindgen_7e61df7271c4ff58" hs_bindgen_7e61df7271c4ff58_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
hs_bindgen_7e61df7271c4ff58 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_7e61df7271c4ff58 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7e61df7271c4ff58_base

{-# NOINLINE hs_bindgen_7af49c80665b9a25 #-}
{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h 29:18@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_cϒϒϒ@
-}
hs_bindgen_7af49c80665b9a25 :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
hs_bindgen_7af49c80665b9a25 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e61df7271c4ff58

{-# NOINLINE cϒϒϒ #-}
cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_7af49c80665b9a25))
