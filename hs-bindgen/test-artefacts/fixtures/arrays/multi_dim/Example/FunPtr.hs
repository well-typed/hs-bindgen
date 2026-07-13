{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.foo_const
    , Example.FunPtr.bar
    , Example.FunPtr.bar_const
    , Example.FunPtr.baz
    , Example.FunPtr.baz_const
    , Example.FunPtr.quuz
    , Example.FunPtr.quuz_const
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <arrays/multi_dim.h>"
  , "/* test_arraysmulti_dim_Example_get_foo */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_02d97ecc46bbc8e0 (void)) ("
  , "  signed int (*arg1)[4]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_foo_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8baaa4bee7bf2bf7 (void)) ("
  , "  signed int const (*arg1)[4]"
  , ")"
  , "{"
  , "  return &foo_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_27d306f03bbe4aeb (void)) ("
  , "  signed int (*arg1)[2]"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_bar_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_dedcf308a5d44f1d (void)) ("
  , "  signed int const (*arg1)[2]"
  , ")"
  , "{"
  , "  return &bar_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_baz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f8c450c8715a6834 (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_baz_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_520d9475875f88b2 (void)) ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return &baz_const;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_quuz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_cb1714c714867d37 (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &quuz;"
  , "}"
  , "/* test_arraysmulti_dim_Example_get_quuz_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b4e5ee141d4c7703 (void)) ("
  , "  signed int const (*arg1)[3]"
  , ")"
  , "{"
  , "  return &quuz_const;"
  , "}"
  ]))

-- __unique:__ @test_arraysmulti_dim_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_02d97ecc46bbc8e0" hs_bindgen_02d97ecc46bbc8e0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_foo@
hs_bindgen_02d97ecc46bbc8e0 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt))) -> IO BG.CInt))
hs_bindgen_02d97ecc46bbc8e0 =
  BG.fromFFIType hs_bindgen_02d97ecc46bbc8e0_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @arrays\/multi_dim.h 4:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt))) -> IO BG.CInt)
foo = BG.unsafePerformIO hs_bindgen_02d97ecc46bbc8e0

-- __unique:__ @test_arraysmulti_dim_Example_get_foo_const@
foreign import ccall unsafe "hs_bindgen_8baaa4bee7bf2bf7" hs_bindgen_8baaa4bee7bf2bf7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_foo_const@
hs_bindgen_8baaa4bee7bf2bf7 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt))) -> IO BG.CInt))
hs_bindgen_8baaa4bee7bf2bf7 =
  BG.fromFFIType hs_bindgen_8baaa4bee7bf2bf7_base

{-# NOINLINE foo_const #-}
{-| __C declaration:__ @foo_const@

    __defined at:__ @arrays\/multi_dim.h 5:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
foo_const :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 (CA.ConstantArray 4 BG.CInt))) -> IO BG.CInt)
foo_const =
  BG.unsafePerformIO hs_bindgen_8baaa4bee7bf2bf7

-- __unique:__ @test_arraysmulti_dim_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_27d306f03bbe4aeb" hs_bindgen_27d306f03bbe4aeb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_bar@
hs_bindgen_27d306f03bbe4aeb :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt))) -> IO BG.CInt))
hs_bindgen_27d306f03bbe4aeb =
  BG.fromFFIType hs_bindgen_27d306f03bbe4aeb_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @arrays\/multi_dim.h 8:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt))) -> IO BG.CInt)
bar = BG.unsafePerformIO hs_bindgen_27d306f03bbe4aeb

-- __unique:__ @test_arraysmulti_dim_Example_get_bar_const@
foreign import ccall unsafe "hs_bindgen_dedcf308a5d44f1d" hs_bindgen_dedcf308a5d44f1d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_bar_const@
hs_bindgen_dedcf308a5d44f1d :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt))) -> IO BG.CInt))
hs_bindgen_dedcf308a5d44f1d =
  BG.fromFFIType hs_bindgen_dedcf308a5d44f1d_base

{-# NOINLINE bar_const #-}
{-| __C declaration:__ @bar_const@

    __defined at:__ @arrays\/multi_dim.h 9:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
bar_const :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 2 BG.CInt))) -> IO BG.CInt)
bar_const =
  BG.unsafePerformIO hs_bindgen_dedcf308a5d44f1d

-- __unique:__ @test_arraysmulti_dim_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_f8c450c8715a6834" hs_bindgen_f8c450c8715a6834_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_baz@
hs_bindgen_f8c450c8715a6834 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> IO BG.CInt))
hs_bindgen_f8c450c8715a6834 =
  BG.fromFFIType hs_bindgen_f8c450c8715a6834_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @arrays\/multi_dim.h 13:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz :: BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> IO BG.CInt)
baz = BG.unsafePerformIO hs_bindgen_f8c450c8715a6834

-- __unique:__ @test_arraysmulti_dim_Example_get_baz_const@
foreign import ccall unsafe "hs_bindgen_520d9475875f88b2" hs_bindgen_520d9475875f88b2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_baz_const@
hs_bindgen_520d9475875f88b2 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem Matrix) -> IO BG.CInt))
hs_bindgen_520d9475875f88b2 =
  BG.fromFFIType hs_bindgen_520d9475875f88b2_base

{-# NOINLINE baz_const #-}
{-| __C declaration:__ @baz_const@

    __defined at:__ @arrays\/multi_dim.h 14:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
baz_const :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem Matrix) -> IO BG.CInt)
baz_const =
  BG.unsafePerformIO hs_bindgen_520d9475875f88b2

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz@
foreign import ccall unsafe "hs_bindgen_cb1714c714867d37" hs_bindgen_cb1714c714867d37_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz@
hs_bindgen_cb1714c714867d37 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Triplets) -> IO BG.CInt))
hs_bindgen_cb1714c714867d37 =
  BG.fromFFIType hs_bindgen_cb1714c714867d37_base

{-# NOINLINE quuz #-}
{-| __C declaration:__ @quuz@

    __defined at:__ @arrays\/multi_dim.h 18:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz :: BG.FunPtr (BG.Ptr (IsA.Elem Triplets) -> IO BG.CInt)
quuz = BG.unsafePerformIO hs_bindgen_cb1714c714867d37

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz_const@
foreign import ccall unsafe "hs_bindgen_b4e5ee141d4c7703" hs_bindgen_b4e5ee141d4c7703_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysmulti_dim_Example_get_quuz_const@
hs_bindgen_b4e5ee141d4c7703 :: IO (BG.FunPtr (PtrConst.PtrConst (IsA.Elem Triplets) -> IO BG.CInt))
hs_bindgen_b4e5ee141d4c7703 =
  BG.fromFFIType hs_bindgen_b4e5ee141d4c7703_base

{-# NOINLINE quuz_const #-}
{-| __C declaration:__ @quuz_const@

    __defined at:__ @arrays\/multi_dim.h 19:5@

    __exported by:__ @arrays\/multi_dim.h@
-}
quuz_const :: BG.FunPtr (PtrConst.PtrConst (IsA.Elem Triplets) -> IO BG.CInt)
quuz_const =
  BG.unsafePerformIO hs_bindgen_b4e5ee141d4c7703
