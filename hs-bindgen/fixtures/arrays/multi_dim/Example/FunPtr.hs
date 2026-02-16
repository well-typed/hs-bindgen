{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/multi_dim.h>"
  , "/* test_arraysmulti_dim_Example_get_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_02d97ecc46bbc8e0 (void)) ("
  , "  signed int arg1[3][4]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_foo_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8baaa4bee7bf2bf7 (void)) ("
  , "  signed int const arg1[3][4]"
  , ")"
  , "{"
  , "  return &foo_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_27d306f03bbe4aeb (void)) ("
  , "  signed int arg1[][2]"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_bar_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_dedcf308a5d44f1d (void)) ("
  , "  signed int const arg1[][2]"
  , ")"
  , "{"
  , "  return &bar_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_baz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f8c450c8715a6834 (void)) ("
  , "  matrix arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_baz_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_520d9475875f88b2 (void)) ("
  , "  matrix const arg1"
  , ")"
  , "{"
  , "  return &baz_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_quuz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cb1714c714867d37 (void)) ("
  , "  triplets arg1"
  , ")"
  , "{"
  , "  return &quuz;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_quuz_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b4e5ee141d4c7703 (void)) ("
  , "  triplets const arg1"
  , ")"
  , "{"
  , "  return &quuz_const;"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_02d97ecc46bbc8e0" hs_bindgen_02d97ecc46bbc8e0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_foo@
hs_bindgen_02d97ecc46bbc8e0 :: IO (RIP.FunPtr (((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt)) -> IO RIP.CInt))
hs_bindgen_02d97ecc46bbc8e0 =
  RIP.fromFFIType hs_bindgen_02d97ecc46bbc8e0_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo :: RIP.FunPtr (((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt)) -> IO RIP.CInt)
foo = RIP.unsafePerformIO hs_bindgen_02d97ecc46bbc8e0

-- __unique:__ @test_arraysmulti_dim_Example_get_foo_const@
foreign import ccall unsafe "hs_bindgen_8baaa4bee7bf2bf7" hs_bindgen_8baaa4bee7bf2bf7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_foo_const@
hs_bindgen_8baaa4bee7bf2bf7 :: IO (RIP.FunPtr (((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt)) -> IO RIP.CInt))
hs_bindgen_8baaa4bee7bf2bf7 =
  RIP.fromFFIType hs_bindgen_8baaa4bee7bf2bf7_base

{-# NOINLINE foo_const #-}
{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const :: RIP.FunPtr (((CA.ConstantArray 3) ((CA.ConstantArray 4) RIP.CInt)) -> IO RIP.CInt)
foo_const =
  RIP.unsafePerformIO hs_bindgen_8baaa4bee7bf2bf7

-- __unique:__ @test_arraysmulti_dim_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_27d306f03bbe4aeb" hs_bindgen_27d306f03bbe4aeb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_bar@
hs_bindgen_27d306f03bbe4aeb :: IO (RIP.FunPtr ((IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt)) -> IO RIP.CInt))
hs_bindgen_27d306f03bbe4aeb =
  RIP.fromFFIType hs_bindgen_27d306f03bbe4aeb_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar :: RIP.FunPtr ((IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt)) -> IO RIP.CInt)
bar = RIP.unsafePerformIO hs_bindgen_27d306f03bbe4aeb

-- __unique:__ @test_arraysmulti_dim_Example_get_bar_const@
foreign import ccall unsafe "hs_bindgen_dedcf308a5d44f1d" hs_bindgen_dedcf308a5d44f1d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_bar_const@
hs_bindgen_dedcf308a5d44f1d :: IO (RIP.FunPtr ((IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt)) -> IO RIP.CInt))
hs_bindgen_dedcf308a5d44f1d =
  RIP.fromFFIType hs_bindgen_dedcf308a5d44f1d_base

{-# NOINLINE bar_const #-}
{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const :: RIP.FunPtr ((IA.IncompleteArray ((CA.ConstantArray 2) RIP.CInt)) -> IO RIP.CInt)
bar_const =
  RIP.unsafePerformIO hs_bindgen_dedcf308a5d44f1d

-- __unique:__ @test_arraysmulti_dim_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_f8c450c8715a6834" hs_bindgen_f8c450c8715a6834_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_baz@
hs_bindgen_f8c450c8715a6834 :: IO (RIP.FunPtr (Matrix -> IO RIP.CInt))
hs_bindgen_f8c450c8715a6834 =
  RIP.fromFFIType hs_bindgen_f8c450c8715a6834_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz :: RIP.FunPtr (Matrix -> IO RIP.CInt)
baz = RIP.unsafePerformIO hs_bindgen_f8c450c8715a6834

-- __unique:__ @test_arraysmulti_dim_Example_get_baz_const@
foreign import ccall unsafe "hs_bindgen_520d9475875f88b2" hs_bindgen_520d9475875f88b2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_baz_const@
hs_bindgen_520d9475875f88b2 :: IO (RIP.FunPtr (Matrix -> IO RIP.CInt))
hs_bindgen_520d9475875f88b2 =
  RIP.fromFFIType hs_bindgen_520d9475875f88b2_base

{-# NOINLINE baz_const #-}
{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const :: RIP.FunPtr (Matrix -> IO RIP.CInt)
baz_const =
  RIP.unsafePerformIO hs_bindgen_520d9475875f88b2

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz@
foreign import ccall unsafe "hs_bindgen_cb1714c714867d37" hs_bindgen_cb1714c714867d37_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz@
hs_bindgen_cb1714c714867d37 :: IO (RIP.FunPtr (Triplets -> IO RIP.CInt))
hs_bindgen_cb1714c714867d37 =
  RIP.fromFFIType hs_bindgen_cb1714c714867d37_base

{-# NOINLINE quuz #-}
{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz :: RIP.FunPtr (Triplets -> IO RIP.CInt)
quuz =
  RIP.unsafePerformIO hs_bindgen_cb1714c714867d37

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz_const@
foreign import ccall unsafe "hs_bindgen_b4e5ee141d4c7703" hs_bindgen_b4e5ee141d4c7703_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz_const@
hs_bindgen_b4e5ee141d4c7703 :: IO (RIP.FunPtr (Triplets -> IO RIP.CInt))
hs_bindgen_b4e5ee141d4c7703 =
  RIP.fromFFIType hs_bindgen_b4e5ee141d4c7703_base

{-# NOINLINE quuz_const #-}
{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const :: RIP.FunPtr (Triplets -> IO RIP.CInt)
quuz_const =
  RIP.unsafePerformIO hs_bindgen_b4e5ee141d4c7703
