{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/arrays.h>"
  , "/* test_manualarrays_Example_get_transpose */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c99d8a3b0363a0fa (void)) ("
  , "  triplet const *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  return &transpose;"
  , "}"
  , "/* test_manualarrays_Example_get_pretty_print_triplets */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_923b33cbbfeb5e7d (void)) ("
  , "  signed int (**arg1)[3]"
  , ")"
  , "{"
  , "  return &pretty_print_triplets;"
  , "}"
  ]))

-- __unique:__ @test_manualarrays_Example_get_transpose@
foreign import ccall unsafe "hs_bindgen_c99d8a3b0363a0fa" hs_bindgen_c99d8a3b0363a0fa_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_transpose@
hs_bindgen_c99d8a3b0363a0fa :: IO (RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem Matrix)) -> (RIP.Ptr (IsA.Elem Matrix)) -> IO ()))
hs_bindgen_c99d8a3b0363a0fa =
  RIP.fromFFIType hs_bindgen_c99d8a3b0363a0fa_base

{-# NOINLINE transpose #-}
{-| __C declaration:__ @transpose@

    __defined at:__ @manual\/arrays.h 36:6@

    __exported by:__ @manual\/arrays.h@
-}
transpose :: RIP.FunPtr ((PtrConst.PtrConst (IsA.Elem Matrix)) -> (RIP.Ptr (IsA.Elem Matrix)) -> IO ())
transpose =
  RIP.unsafePerformIO hs_bindgen_c99d8a3b0363a0fa

-- __unique:__ @test_manualarrays_Example_get_pretty_print_triplets@
foreign import ccall unsafe "hs_bindgen_923b33cbbfeb5e7d" hs_bindgen_923b33cbbfeb5e7d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_manualarrays_Example_get_pretty_print_triplets@
hs_bindgen_923b33cbbfeb5e7d :: IO (RIP.FunPtr ((RIP.Ptr (IsA.Elem Triplet_ptrs)) -> IO ()))
hs_bindgen_923b33cbbfeb5e7d =
  RIP.fromFFIType hs_bindgen_923b33cbbfeb5e7d_base

{-# NOINLINE pretty_print_triplets #-}
{-| A function that prints the given triplet_ptrs

__C declaration:__ @pretty_print_triplets@

__defined at:__ @manual\/arrays.h 50:13@

__exported by:__ @manual\/arrays.h@
-}
pretty_print_triplets :: RIP.FunPtr ((RIP.Ptr (IsA.Elem Triplet_ptrs)) -> IO ())
pretty_print_triplets =
  RIP.unsafePerformIO hs_bindgen_923b33cbbfeb5e7d
