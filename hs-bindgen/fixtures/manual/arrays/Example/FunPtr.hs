{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Internal.CAPI
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "/* test_manualarrays_Example_get_transpose */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c99d8a3b0363a0fa (void)) ("
  , "  matrix const arg1,"
  , "  matrix arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  , "/* test_manualarrays_Example_get_pretty_print_triplets */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_923b33cbbfeb5e7d (void)) ("
  , "  triplet_ptrs arg1"
  , ")"
  , "{"
  , "  return &pretty_print_triplets;"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_get_transpose@
foreign import ccall unsafe "hs_bindgen_c99d8a3b0363a0fa" hs_bindgen_c99d8a3b0363a0fa_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualarrays_Example_get_transpose@
hs_bindgen_c99d8a3b0363a0fa :: IO (Ptr.FunPtr (Matrix -> Matrix -> IO ()))
hs_bindgen_c99d8a3b0363a0fa =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c99d8a3b0363a0fa_base

{-# NOINLINE transpose #-}
{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose :: Ptr.FunPtr (Matrix -> Matrix -> IO ())
transpose =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c99d8a3b0363a0fa

-- __unique:__ @test_manualarrays_Example_get_pretty_print_triplets@
foreign import ccall unsafe "hs_bindgen_923b33cbbfeb5e7d" hs_bindgen_923b33cbbfeb5e7d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_manualarrays_Example_get_pretty_print_triplets@
hs_bindgen_923b33cbbfeb5e7d :: IO (Ptr.FunPtr (Triplet_ptrs -> IO ()))
hs_bindgen_923b33cbbfeb5e7d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_923b33cbbfeb5e7d_base

{-# NOINLINE pretty_print_triplets #-}
{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets :: Ptr.FunPtr (Triplet_ptrs -> IO ())
pretty_print_triplets =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_923b33cbbfeb5e7d
