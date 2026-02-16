{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒ@
hs_bindgen_aa137b95cfa81f42 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_aa137b95cfa81f42 =
  RIP.fromFFIType hs_bindgen_aa137b95cfa81f42_base

{-# NOINLINE cϒϒ #-}
{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h 26:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ :: RIP.Ptr RIP.CInt
cϒϒ = RIP.unsafePerformIO hs_bindgen_aa137b95cfa81f42

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
foreign import ccall unsafe "hs_bindgen_7e61df7271c4ff58" hs_bindgen_7e61df7271c4ff58_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
hs_bindgen_7e61df7271c4ff58 :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_7e61df7271c4ff58 =
  RIP.fromFFIType hs_bindgen_7e61df7271c4ff58_base

{-# NOINLINE hs_bindgen_7af49c80665b9a25 #-}
{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h 29:18@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_cϒϒϒ@
-}
hs_bindgen_7af49c80665b9a25 :: PtrConst.PtrConst RIP.CInt
hs_bindgen_7af49c80665b9a25 =
  RIP.unsafePerformIO hs_bindgen_7e61df7271c4ff58

{-# NOINLINE cϒϒϒ #-}
cϒϒϒ :: RIP.CInt
cϒϒϒ =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_7af49c80665b9a25)
