{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Prelude (Bounded, Enum, Eq, IO, Integral, Num, Ord, Read, Real, Show)

$(HsBindgen.Runtime.Prelude.addCSource "#include <adios.h>\nvoid hs_bindgen_test_adios_1f928c1e5a3ea8be (void) { \978(); }\nvoid hs_bindgen_test_adios_912e938ac6370f83 (void) { \25308\25308(); }\nvoid hs_bindgen_test_adios_cc7cd7984d0bfaee (void) { Say\25308\25308(); }\n/* get_\978_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_857cc80028e9fd4d (void)) (void) { return &\978; } \n/* get_\25308\25308_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_8b289d4c7ae2c2a7 (void)) (void) { return &\25308\25308; } \n/* get_Say\25308\25308_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_adios_2879b42f75005d3b (void)) (void) { return &Say\25308\25308; } \n/* get_\978\978_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_adios_e4b974661ff038a0 (void) { return &\978\978; } \n/* get_\978\978\978_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_adios_c538a25ba7055dd4 (void) { return &\978\978\978; } \n")

{-| __C declaration:__ @adiós@

    __defined at:__ @adios.h:7:13@

    __exported by:__ @adios.h@
-}
newtype Adio'0301s = Adio'0301s
  { un_Adio'0301s :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @数字@

    __defined at:__ @adios.h:12:13@

    __exported by:__ @adios.h@
-}
newtype C数字 = C数字
  { un_C数字 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @ϒ@

    __defined at:__ @adios.h:18:6@

    __exported by:__ @adios.h@
-}
foreign import ccall safe "hs_bindgen_test_adios_1f928c1e5a3ea8be" cϒ
  :: IO ()

{-| __C declaration:__ @拜拜@

    __defined at:__ @adios.h:27:6@

    __exported by:__ @adios.h@
-}
foreign import ccall safe "hs_bindgen_test_adios_912e938ac6370f83" 拜拜
  :: IO ()

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @adios.h:31:6@

    __exported by:__ @adios.h@
-}
foreign import ccall safe "hs_bindgen_test_adios_cc7cd7984d0bfaee" say拜拜
  :: IO ()

foreign import ccall unsafe "hs_bindgen_test_adios_857cc80028e9fd4d" hs_bindgen_test_adios_857cc80028e9fd4d
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cϒ_ptr #-}

{-| __C declaration:__ @ϒ@

    __defined at:__ @adios.h:18:6@

    __exported by:__ @adios.h@
-}
cϒ_ptr :: Ptr.FunPtr (IO ())
cϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_857cc80028e9fd4d

foreign import ccall unsafe "hs_bindgen_test_adios_8b289d4c7ae2c2a7" hs_bindgen_test_adios_8b289d4c7ae2c2a7
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE 拜拜_ptr #-}

{-| __C declaration:__ @拜拜@

    __defined at:__ @adios.h:27:6@

    __exported by:__ @adios.h@
-}
拜拜_ptr :: Ptr.FunPtr (IO ())
拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_8b289d4c7ae2c2a7

foreign import ccall unsafe "hs_bindgen_test_adios_2879b42f75005d3b" hs_bindgen_test_adios_2879b42f75005d3b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE say拜拜_ptr #-}

{-| __C declaration:__ @Say拜拜@

    __defined at:__ @adios.h:31:6@

    __exported by:__ @adios.h@
-}
say拜拜_ptr :: Ptr.FunPtr (IO ())
say拜拜_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_2879b42f75005d3b

foreign import ccall unsafe "hs_bindgen_test_adios_e4b974661ff038a0" hs_bindgen_test_adios_e4b974661ff038a0
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒ@

    __defined at:__ @adios.h:21:12@

    __exported by:__ @adios.h@
-}
cϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_e4b974661ff038a0

foreign import ccall unsafe "hs_bindgen_test_adios_c538a25ba7055dd4" hs_bindgen_test_adios_c538a25ba7055dd4
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE cϒϒϒ_ptr #-}

{-| __C declaration:__ @ϒϒϒ@

    __defined at:__ @adios.h:24:18@

    __exported by:__ @adios.h@
-}
cϒϒϒ_ptr :: Ptr.Ptr FC.CInt
cϒϒϒ_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_adios_c538a25ba7055dd4

{-# NOINLINE cϒϒϒ #-}

cϒϒϒ :: FC.CInt
cϒϒϒ =
  GHC.IO.Unsafe.unsafePerformIO (F.peek cϒϒϒ_ptr)
