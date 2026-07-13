{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.cϒϒ
    , Example.Global.cϒϒϒ
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒ@
hs_bindgen_aa137b95cfa81f42 :: IO (BG.Ptr BG.CInt)
hs_bindgen_aa137b95cfa81f42 =
  BG.fromFFIType hs_bindgen_aa137b95cfa81f42_base

{-# NOINLINE cϒϒ #-}
{-| __C declaration:__ @ϒϒ@

    __defined at:__ @edge-cases\/adios.h 26:12@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒϒ :: BG.Ptr BG.CInt
cϒϒ = BG.unsafePerformIO hs_bindgen_aa137b95cfa81f42

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
foreign import ccall unsafe "hs_bindgen_7e61df7271c4ff58" hs_bindgen_7e61df7271c4ff58_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesadios_Example_get_ϒϒϒ@
hs_bindgen_7e61df7271c4ff58 :: IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_7e61df7271c4ff58 =
  BG.fromFFIType hs_bindgen_7e61df7271c4ff58_base

{-# NOINLINE hs_bindgen_7af49c80665b9a25 #-}
{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @edge-cases\/adios.h 29:18@

    __exported by:__ @edge-cases\/adios.h@

    __unique:__ @test_edgecasesadios_Example_cϒϒϒ@
-}
hs_bindgen_7af49c80665b9a25 :: PtrConst.PtrConst BG.CInt
hs_bindgen_7af49c80665b9a25 =
  BG.unsafePerformIO hs_bindgen_7e61df7271c4ff58

{-# NOINLINE cϒϒϒ #-}
cϒϒϒ :: BG.CInt
cϒϒϒ =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_7af49c80665b9a25)
