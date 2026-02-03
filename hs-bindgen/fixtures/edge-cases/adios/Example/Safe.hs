{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/adios.h>"
  , "signed int hs_bindgen_2f6d4be143076044 (void)"
  , "{"
  , "  return adio\769s_fun();"
  , "}"
  , "void hs_bindgen_2010521804ef9a6e (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_3bc3e53cc82c9580 (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_ad8eb47027b2d49d (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

-- __unique:__ @test_edgecasesadios_Example_Safe_adiós_fun@
foreign import ccall safe "hs_bindgen_2f6d4be143076044" hs_bindgen_2f6d4be143076044_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_edgecasesadios_Example_Safe_adiós_fun@
hs_bindgen_2f6d4be143076044 :: IO FC.CInt
hs_bindgen_2f6d4be143076044 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2f6d4be143076044_base

{-| __C declaration:__ @adiós_fun@

    __defined at:__ @edge-cases\/adios.h 12:5@

    __exported by:__ @edge-cases\/adios.h@
-}
adio'0301s_fun :: IO FC.CInt
adio'0301s_fun = hs_bindgen_2f6d4be143076044

-- __unique:__ @test_edgecasesadios_Example_Safe_ϒ@
foreign import ccall safe "hs_bindgen_2010521804ef9a6e" hs_bindgen_2010521804ef9a6e_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Safe_ϒ@
hs_bindgen_2010521804ef9a6e :: IO ()
hs_bindgen_2010521804ef9a6e =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_2010521804ef9a6e_base

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h 23:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ :: IO ()
cϒ = hs_bindgen_2010521804ef9a6e

-- __unique:__ @test_edgecasesadios_Example_Safe_拜拜@
foreign import ccall safe "hs_bindgen_3bc3e53cc82c9580" hs_bindgen_3bc3e53cc82c9580_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Safe_拜拜@
hs_bindgen_3bc3e53cc82c9580 :: IO ()
hs_bindgen_3bc3e53cc82c9580 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_3bc3e53cc82c9580_base

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h 32:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜 :: IO ()
拜拜 = hs_bindgen_3bc3e53cc82c9580

-- __unique:__ @test_edgecasesadios_Example_Safe_Say拜拜@
foreign import ccall safe "hs_bindgen_ad8eb47027b2d49d" hs_bindgen_ad8eb47027b2d49d_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Safe_Say拜拜@
hs_bindgen_ad8eb47027b2d49d :: IO ()
hs_bindgen_ad8eb47027b2d49d =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_ad8eb47027b2d49d_base

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h 36:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜 :: IO ()
say拜拜 = hs_bindgen_ad8eb47027b2d49d
