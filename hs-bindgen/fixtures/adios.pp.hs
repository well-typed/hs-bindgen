{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(CAPI.addCSource "#include <adios.h>\nvoid hs_bindgen_test_adios_8e1936b23d816eb2 (void) { \978(); }\n/* get_\978_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_64cbec67bc73ad5c (void)) (void) { return &\978; } \n/* get_\978\978_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_adios_52f5e750c2f31c7b (void) { return &\978\978; } \n/* get_\978\978\978_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_adios_13030842ed540098 (void) { return &\978\978\978; } \nvoid hs_bindgen_test_adios_5c74896d56245684 (void) { \25308\25308(); }\n/* get_\25308\25308_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_8472427dddbc4eb1 (void)) (void) { return &\25308\25308; } \nvoid hs_bindgen_test_adios_e8498bfc0fabc9e9 (void) { Say\25308\25308(); }\n/* get_Say\25308\25308_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_7b73f645a5d28e6b (void)) (void) { return &Say\25308\25308; } \n")

newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __from C:__ @ϒ@ -}
foreign import ccall safe "hs_bindgen_test_adios_8e1936b23d816eb2" cϒ
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_adios_64cbec67bc73ad5c" hs_bindgen_test_adios_64cbec67bc73ad5c
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE cϒ_ptr #-}

cϒ_ptr :: F.FunPtr (IO ())
cϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_64cbec67bc73ad5c

foreign import ccall unsafe "hs_bindgen_test_adios_52f5e750c2f31c7b" hs_bindgen_test_adios_52f5e750c2f31c7b
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE cϒϒ_ptr #-}

cϒϒ_ptr :: F.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_52f5e750c2f31c7b

foreign import ccall unsafe "hs_bindgen_test_adios_13030842ed540098" hs_bindgen_test_adios_13030842ed540098
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE cϒϒϒ_ptr #-}

cϒϒϒ_ptr :: F.Ptr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_13030842ed540098

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek cϒϒϒ_ptr)

{-| __from C:__ @拜拜@ -}
foreign import ccall safe "hs_bindgen_test_adios_5c74896d56245684" 拜拜
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_adios_8472427dddbc4eb1" hs_bindgen_test_adios_8472427dddbc4eb1
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE 拜拜_ptr #-}

拜拜_ptr :: F.FunPtr (IO ())
拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_8472427dddbc4eb1

{-| __from C:__ @Say拜拜@ -}
foreign import ccall safe "hs_bindgen_test_adios_e8498bfc0fabc9e9" say拜拜
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_adios_7b73f645a5d28e6b" hs_bindgen_test_adios_7b73f645a5d28e6b
  :: IO (F.FunPtr (IO ()))

{-# NOINLINE say拜拜_ptr #-}

say拜拜_ptr :: F.FunPtr (IO ())
say拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_7b73f645a5d28e6b
