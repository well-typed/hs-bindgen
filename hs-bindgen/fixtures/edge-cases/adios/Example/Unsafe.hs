{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/adios.h>"
  , "signed int hs_bindgen_2a3071850c230aa3 (void)"
  , "{"
  , "  return adio\769s_fun();"
  , "}"
  , "void hs_bindgen_1814d14d59d9daf7 (void)"
  , "{"
  , "  \978();"
  , "}"
  , "void hs_bindgen_c1ab9527e537714b (void)"
  , "{"
  , "  \25308\25308();"
  , "}"
  , "void hs_bindgen_d532055af9051fad (void)"
  , "{"
  , "  Say\25308\25308();"
  , "}"
  ]))

-- __unique:__ @test_edgecasesadios_Example_Unsafe_adiós_fun@
foreign import ccall unsafe "hs_bindgen_2a3071850c230aa3" hs_bindgen_2a3071850c230aa3_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_edgecasesadios_Example_Unsafe_adiós_fun@
hs_bindgen_2a3071850c230aa3 :: IO FC.CInt
hs_bindgen_2a3071850c230aa3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2a3071850c230aa3_base

{-| __C declaration:__ @adiós_fun@

    __defined at:__ @edge-cases\/adios.h 12:5@

    __exported by:__ @edge-cases\/adios.h@
-}
adio'0301s_fun :: IO FC.CInt
adio'0301s_fun = hs_bindgen_2a3071850c230aa3

-- __unique:__ @test_edgecasesadios_Example_Unsafe_ϒ@
foreign import ccall unsafe "hs_bindgen_1814d14d59d9daf7" hs_bindgen_1814d14d59d9daf7_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Unsafe_ϒ@
hs_bindgen_1814d14d59d9daf7 :: IO ()
hs_bindgen_1814d14d59d9daf7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1814d14d59d9daf7_base

{-| __C declaration:__ @ϒ@

    __defined at:__ @edge-cases\/adios.h 23:6@

    __exported by:__ @edge-cases\/adios.h@
-}
cϒ :: IO ()
cϒ = hs_bindgen_1814d14d59d9daf7

-- __unique:__ @test_edgecasesadios_Example_Unsafe_拜拜@
foreign import ccall unsafe "hs_bindgen_c1ab9527e537714b" hs_bindgen_c1ab9527e537714b_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Unsafe_拜拜@
hs_bindgen_c1ab9527e537714b :: IO ()
hs_bindgen_c1ab9527e537714b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c1ab9527e537714b_base

{-| __C declaration:__ @拜拜@

    __defined at:__ @edge-cases\/adios.h 32:6@

    __exported by:__ @edge-cases\/adios.h@
-}
拜拜 :: IO ()
拜拜 = hs_bindgen_c1ab9527e537714b

-- __unique:__ @test_edgecasesadios_Example_Unsafe_Say拜拜@
foreign import ccall unsafe "hs_bindgen_d532055af9051fad" hs_bindgen_d532055af9051fad_base ::
     IO ()

-- __unique:__ @test_edgecasesadios_Example_Unsafe_Say拜拜@
hs_bindgen_d532055af9051fad :: IO ()
hs_bindgen_d532055af9051fad =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d532055af9051fad_base

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @edge-cases\/adios.h 36:6@

    __exported by:__ @edge-cases\/adios.h@
-}
say拜拜 :: IO ()
say拜拜 = hs_bindgen_d532055af9051fad
