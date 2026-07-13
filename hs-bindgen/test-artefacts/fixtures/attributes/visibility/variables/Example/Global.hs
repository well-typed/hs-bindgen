{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.i0
    , Example.Global.i1
    , Example.Global.i2
    , Example.Global.i3
    , Example.Global.i4
    , Example.Global.i5
    , Example.Global.i6
    , Example.Global.i7
    , Example.Global.i8
    , Example.Global.i9
    , Example.Global.i10
    , Example.Global.i11
    , Example.Global.i12
    , Example.Global.i13
    , Example.Global.i14
    , Example.Global.i15
    , Example.Global.i16
    , Example.Global.i17
    , Example.Global.i18
    , Example.Global.i19
    , Example.Global.i20
    , Example.Global.i21
    , Example.Global.i22
    , Example.Global.i23
    , Example.Global.i24
    , Example.Global.i25
    , Example.Global.i26
    , Example.Global.i27
    , Example.Global.i28
    , Example.Global.i29
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <attributes/visibility/variables.h>"
  , "/* test_attributesvisibilityvariable_Example_get_i0 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_39a034063990df12 (void)"
  , "{"
  , "  return &i0;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_083fddca08b7aced (void)"
  , "{"
  , "  return &i1;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i2 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a76894155b8a6ec7 (void)"
  , "{"
  , "  return &i2;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i3 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_ec3a25d8132ea504 (void)"
  , "{"
  , "  return &i3;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i4 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_45d491ce87ed46d0 (void)"
  , "{"
  , "  return &i4;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i5 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_fe19c8a8ebc1d947 (void)"
  , "{"
  , "  return &i5;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i6 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c1a332634f600e94 (void)"
  , "{"
  , "  return &i6;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i7 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c9bd5e0c60ff8f87 (void)"
  , "{"
  , "  return &i7;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i8 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_94e7124f7562b1d3 (void)"
  , "{"
  , "  return &i8;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i9 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_ec473304783162de (void)"
  , "{"
  , "  return &i9;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i10 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8756e67db8a0a542 (void)"
  , "{"
  , "  return &i10;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i11 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_0106cc7a5825bf8b (void)"
  , "{"
  , "  return &i11;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i12 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_296af89863107edf (void)"
  , "{"
  , "  return &i12;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i13 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_ddfe303b28effdbb (void)"
  , "{"
  , "  return &i13;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i14 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_b3911569cdb1adcc (void)"
  , "{"
  , "  return &i14;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i15 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_94b1cd9806ce5bcc (void)"
  , "{"
  , "  return &i15;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i16 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_e667df4994fab2af (void)"
  , "{"
  , "  return &i16;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i17 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_656c7420378b3f41 (void)"
  , "{"
  , "  return &i17;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i18 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_d60425923e1c6414 (void)"
  , "{"
  , "  return &i18;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i19 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_cb11e511be0cfc50 (void)"
  , "{"
  , "  return &i19;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i20 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_8720bde7cc30b171 (void)"
  , "{"
  , "  return &i20;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i21 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_b71914b49e6bc6d8 (void)"
  , "{"
  , "  return &i21;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i22 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_22b6f19b3356e742 (void)"
  , "{"
  , "  return &i22;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i23 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_4432bdff99ca3ab2 (void)"
  , "{"
  , "  return &i23;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i24 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_80552dd326e314dd (void)"
  , "{"
  , "  return &i24;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i25 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_e782a3b4c5c62b6d (void)"
  , "{"
  , "  return &i25;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i26 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_9888ce2162201152 (void)"
  , "{"
  , "  return &i26;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i27 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_985cecf4c0c11aee (void)"
  , "{"
  , "  return &i27;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i28 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_b02407ada9cb4466 (void)"
  , "{"
  , "  return &i28;"
  , "}"
  , "/* test_attributesvisibilityvariable_Example_get_i29 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_dc2295e107352a77 (void)"
  , "{"
  , "  return &i29;"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i0@
foreign import ccall unsafe "hs_bindgen_39a034063990df12" hs_bindgen_39a034063990df12_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i0@
hs_bindgen_39a034063990df12 :: IO (BG.Ptr BG.CInt)
hs_bindgen_39a034063990df12 =
  BG.fromFFIType hs_bindgen_39a034063990df12_base

{-# NOINLINE i0 #-}
{-| __C declaration:__ @i0@

    __defined at:__ @attributes\/visibility\/variables.h 14:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i0 :: BG.Ptr BG.CInt
i0 = BG.unsafePerformIO hs_bindgen_39a034063990df12

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i1@
foreign import ccall unsafe "hs_bindgen_083fddca08b7aced" hs_bindgen_083fddca08b7aced_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i1@
hs_bindgen_083fddca08b7aced :: IO (BG.Ptr BG.CInt)
hs_bindgen_083fddca08b7aced =
  BG.fromFFIType hs_bindgen_083fddca08b7aced_base

{-# NOINLINE i1 #-}
{-| __C declaration:__ @i1@

    __defined at:__ @attributes\/visibility\/variables.h 15:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i1 :: BG.Ptr BG.CInt
i1 = BG.unsafePerformIO hs_bindgen_083fddca08b7aced

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i2@
foreign import ccall unsafe "hs_bindgen_a76894155b8a6ec7" hs_bindgen_a76894155b8a6ec7_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i2@
hs_bindgen_a76894155b8a6ec7 :: IO (BG.Ptr BG.CInt)
hs_bindgen_a76894155b8a6ec7 =
  BG.fromFFIType hs_bindgen_a76894155b8a6ec7_base

{-# NOINLINE i2 #-}
{-| __C declaration:__ @i2@

    __defined at:__ @attributes\/visibility\/variables.h 16:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i2 :: BG.Ptr BG.CInt
i2 = BG.unsafePerformIO hs_bindgen_a76894155b8a6ec7

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i3@
foreign import ccall unsafe "hs_bindgen_ec3a25d8132ea504" hs_bindgen_ec3a25d8132ea504_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i3@
hs_bindgen_ec3a25d8132ea504 :: IO (BG.Ptr BG.CInt)
hs_bindgen_ec3a25d8132ea504 =
  BG.fromFFIType hs_bindgen_ec3a25d8132ea504_base

{-# NOINLINE i3 #-}
{-| __C declaration:__ @i3@

    __defined at:__ @attributes\/visibility\/variables.h 17:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i3 :: BG.Ptr BG.CInt
i3 = BG.unsafePerformIO hs_bindgen_ec3a25d8132ea504

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i4@
foreign import ccall unsafe "hs_bindgen_45d491ce87ed46d0" hs_bindgen_45d491ce87ed46d0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i4@
hs_bindgen_45d491ce87ed46d0 :: IO (BG.Ptr BG.CInt)
hs_bindgen_45d491ce87ed46d0 =
  BG.fromFFIType hs_bindgen_45d491ce87ed46d0_base

{-# NOINLINE i4 #-}
{-| __C declaration:__ @i4@

    __defined at:__ @attributes\/visibility\/variables.h 18:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i4 :: BG.Ptr BG.CInt
i4 = BG.unsafePerformIO hs_bindgen_45d491ce87ed46d0

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i5@
foreign import ccall unsafe "hs_bindgen_fe19c8a8ebc1d947" hs_bindgen_fe19c8a8ebc1d947_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i5@
hs_bindgen_fe19c8a8ebc1d947 :: IO (BG.Ptr BG.CInt)
hs_bindgen_fe19c8a8ebc1d947 =
  BG.fromFFIType hs_bindgen_fe19c8a8ebc1d947_base

{-# NOINLINE i5 #-}
{-| __C declaration:__ @i5@

    __defined at:__ @attributes\/visibility\/variables.h 21:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i5 :: BG.Ptr BG.CInt
i5 = BG.unsafePerformIO hs_bindgen_fe19c8a8ebc1d947

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i6@
foreign import ccall unsafe "hs_bindgen_c1a332634f600e94" hs_bindgen_c1a332634f600e94_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i6@
hs_bindgen_c1a332634f600e94 :: IO (BG.Ptr BG.CInt)
hs_bindgen_c1a332634f600e94 =
  BG.fromFFIType hs_bindgen_c1a332634f600e94_base

{-# NOINLINE i6 #-}
{-| __C declaration:__ @i6@

    __defined at:__ @attributes\/visibility\/variables.h 22:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i6 :: BG.Ptr BG.CInt
i6 = BG.unsafePerformIO hs_bindgen_c1a332634f600e94

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i7@
foreign import ccall unsafe "hs_bindgen_c9bd5e0c60ff8f87" hs_bindgen_c9bd5e0c60ff8f87_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i7@
hs_bindgen_c9bd5e0c60ff8f87 :: IO (BG.Ptr BG.CInt)
hs_bindgen_c9bd5e0c60ff8f87 =
  BG.fromFFIType hs_bindgen_c9bd5e0c60ff8f87_base

{-# NOINLINE i7 #-}
{-| __C declaration:__ @i7@

    __defined at:__ @attributes\/visibility\/variables.h 23:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i7 :: BG.Ptr BG.CInt
i7 = BG.unsafePerformIO hs_bindgen_c9bd5e0c60ff8f87

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i8@
foreign import ccall unsafe "hs_bindgen_94e7124f7562b1d3" hs_bindgen_94e7124f7562b1d3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i8@
hs_bindgen_94e7124f7562b1d3 :: IO (BG.Ptr BG.CInt)
hs_bindgen_94e7124f7562b1d3 =
  BG.fromFFIType hs_bindgen_94e7124f7562b1d3_base

{-# NOINLINE i8 #-}
{-| __C declaration:__ @i8@

    __defined at:__ @attributes\/visibility\/variables.h 24:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i8 :: BG.Ptr BG.CInt
i8 = BG.unsafePerformIO hs_bindgen_94e7124f7562b1d3

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i9@
foreign import ccall unsafe "hs_bindgen_ec473304783162de" hs_bindgen_ec473304783162de_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i9@
hs_bindgen_ec473304783162de :: IO (BG.Ptr BG.CInt)
hs_bindgen_ec473304783162de =
  BG.fromFFIType hs_bindgen_ec473304783162de_base

{-# NOINLINE i9 #-}
{-| __C declaration:__ @i9@

    __defined at:__ @attributes\/visibility\/variables.h 25:48@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i9 :: BG.Ptr BG.CInt
i9 = BG.unsafePerformIO hs_bindgen_ec473304783162de

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i10@
foreign import ccall unsafe "hs_bindgen_8756e67db8a0a542" hs_bindgen_8756e67db8a0a542_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i10@
hs_bindgen_8756e67db8a0a542 :: IO (BG.Ptr BG.CInt)
hs_bindgen_8756e67db8a0a542 =
  BG.fromFFIType hs_bindgen_8756e67db8a0a542_base

{-# NOINLINE i10 #-}
{-| __C declaration:__ @i10@

    __defined at:__ @attributes\/visibility\/variables.h 28:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i10 :: BG.Ptr BG.CInt
i10 = BG.unsafePerformIO hs_bindgen_8756e67db8a0a542

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i11@
foreign import ccall unsafe "hs_bindgen_0106cc7a5825bf8b" hs_bindgen_0106cc7a5825bf8b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i11@
hs_bindgen_0106cc7a5825bf8b :: IO (BG.Ptr BG.CInt)
hs_bindgen_0106cc7a5825bf8b =
  BG.fromFFIType hs_bindgen_0106cc7a5825bf8b_base

{-# NOINLINE i11 #-}
{-| __C declaration:__ @i11@

    __defined at:__ @attributes\/visibility\/variables.h 29:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i11 :: BG.Ptr BG.CInt
i11 = BG.unsafePerformIO hs_bindgen_0106cc7a5825bf8b

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i12@
foreign import ccall unsafe "hs_bindgen_296af89863107edf" hs_bindgen_296af89863107edf_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i12@
hs_bindgen_296af89863107edf :: IO (BG.Ptr BG.CInt)
hs_bindgen_296af89863107edf =
  BG.fromFFIType hs_bindgen_296af89863107edf_base

{-# NOINLINE i12 #-}
{-| __C declaration:__ @i12@

    __defined at:__ @attributes\/visibility\/variables.h 30:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i12 :: BG.Ptr BG.CInt
i12 = BG.unsafePerformIO hs_bindgen_296af89863107edf

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i13@
foreign import ccall unsafe "hs_bindgen_ddfe303b28effdbb" hs_bindgen_ddfe303b28effdbb_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i13@
hs_bindgen_ddfe303b28effdbb :: IO (BG.Ptr BG.CInt)
hs_bindgen_ddfe303b28effdbb =
  BG.fromFFIType hs_bindgen_ddfe303b28effdbb_base

{-# NOINLINE i13 #-}
{-| __C declaration:__ @i13@

    __defined at:__ @attributes\/visibility\/variables.h 31:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i13 :: BG.Ptr BG.CInt
i13 = BG.unsafePerformIO hs_bindgen_ddfe303b28effdbb

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i14@
foreign import ccall unsafe "hs_bindgen_b3911569cdb1adcc" hs_bindgen_b3911569cdb1adcc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i14@
hs_bindgen_b3911569cdb1adcc :: IO (BG.Ptr BG.CInt)
hs_bindgen_b3911569cdb1adcc =
  BG.fromFFIType hs_bindgen_b3911569cdb1adcc_base

{-# NOINLINE i14 #-}
{-| __C declaration:__ @i14@

    __defined at:__ @attributes\/visibility\/variables.h 32:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i14 :: BG.Ptr BG.CInt
i14 = BG.unsafePerformIO hs_bindgen_b3911569cdb1adcc

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i15@
foreign import ccall unsafe "hs_bindgen_94b1cd9806ce5bcc" hs_bindgen_94b1cd9806ce5bcc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i15@
hs_bindgen_94b1cd9806ce5bcc :: IO (BG.Ptr BG.CInt)
hs_bindgen_94b1cd9806ce5bcc =
  BG.fromFFIType hs_bindgen_94b1cd9806ce5bcc_base

{-# NOINLINE i15 #-}
{-| __C declaration:__ @i15@

    __defined at:__ @attributes\/visibility\/variables.h 35:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i15 :: BG.Ptr BG.CInt
i15 = BG.unsafePerformIO hs_bindgen_94b1cd9806ce5bcc

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i16@
foreign import ccall unsafe "hs_bindgen_e667df4994fab2af" hs_bindgen_e667df4994fab2af_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i16@
hs_bindgen_e667df4994fab2af :: IO (BG.Ptr BG.CInt)
hs_bindgen_e667df4994fab2af =
  BG.fromFFIType hs_bindgen_e667df4994fab2af_base

{-# NOINLINE i16 #-}
{-| __C declaration:__ @i16@

    __defined at:__ @attributes\/visibility\/variables.h 36:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i16 :: BG.Ptr BG.CInt
i16 = BG.unsafePerformIO hs_bindgen_e667df4994fab2af

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i17@
foreign import ccall unsafe "hs_bindgen_656c7420378b3f41" hs_bindgen_656c7420378b3f41_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i17@
hs_bindgen_656c7420378b3f41 :: IO (BG.Ptr BG.CInt)
hs_bindgen_656c7420378b3f41 =
  BG.fromFFIType hs_bindgen_656c7420378b3f41_base

{-# NOINLINE i17 #-}
{-| __C declaration:__ @i17@

    __defined at:__ @attributes\/visibility\/variables.h 37:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i17 :: BG.Ptr BG.CInt
i17 = BG.unsafePerformIO hs_bindgen_656c7420378b3f41

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i18@
foreign import ccall unsafe "hs_bindgen_d60425923e1c6414" hs_bindgen_d60425923e1c6414_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i18@
hs_bindgen_d60425923e1c6414 :: IO (BG.Ptr BG.CInt)
hs_bindgen_d60425923e1c6414 =
  BG.fromFFIType hs_bindgen_d60425923e1c6414_base

{-# NOINLINE i18 #-}
{-| __C declaration:__ @i18@

    __defined at:__ @attributes\/visibility\/variables.h 38:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i18 :: BG.Ptr BG.CInt
i18 = BG.unsafePerformIO hs_bindgen_d60425923e1c6414

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i19@
foreign import ccall unsafe "hs_bindgen_cb11e511be0cfc50" hs_bindgen_cb11e511be0cfc50_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i19@
hs_bindgen_cb11e511be0cfc50 :: IO (BG.Ptr BG.CInt)
hs_bindgen_cb11e511be0cfc50 =
  BG.fromFFIType hs_bindgen_cb11e511be0cfc50_base

{-# NOINLINE i19 #-}
{-| __C declaration:__ @i19@

    __defined at:__ @attributes\/visibility\/variables.h 39:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i19 :: BG.Ptr BG.CInt
i19 = BG.unsafePerformIO hs_bindgen_cb11e511be0cfc50

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i20@
foreign import ccall unsafe "hs_bindgen_8720bde7cc30b171" hs_bindgen_8720bde7cc30b171_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i20@
hs_bindgen_8720bde7cc30b171 :: IO (BG.Ptr BG.CInt)
hs_bindgen_8720bde7cc30b171 =
  BG.fromFFIType hs_bindgen_8720bde7cc30b171_base

{-# NOINLINE i20 #-}
{-| __C declaration:__ @i20@

    __defined at:__ @attributes\/visibility\/variables.h 42:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i20 :: BG.Ptr BG.CInt
i20 = BG.unsafePerformIO hs_bindgen_8720bde7cc30b171

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i21@
foreign import ccall unsafe "hs_bindgen_b71914b49e6bc6d8" hs_bindgen_b71914b49e6bc6d8_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i21@
hs_bindgen_b71914b49e6bc6d8 :: IO (BG.Ptr BG.CInt)
hs_bindgen_b71914b49e6bc6d8 =
  BG.fromFFIType hs_bindgen_b71914b49e6bc6d8_base

{-# NOINLINE i21 #-}
{-| __C declaration:__ @i21@

    __defined at:__ @attributes\/visibility\/variables.h 43:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i21 :: BG.Ptr BG.CInt
i21 = BG.unsafePerformIO hs_bindgen_b71914b49e6bc6d8

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i22@
foreign import ccall unsafe "hs_bindgen_22b6f19b3356e742" hs_bindgen_22b6f19b3356e742_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i22@
hs_bindgen_22b6f19b3356e742 :: IO (BG.Ptr BG.CInt)
hs_bindgen_22b6f19b3356e742 =
  BG.fromFFIType hs_bindgen_22b6f19b3356e742_base

{-# NOINLINE i22 #-}
{-| __C declaration:__ @i22@

    __defined at:__ @attributes\/visibility\/variables.h 44:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i22 :: BG.Ptr BG.CInt
i22 = BG.unsafePerformIO hs_bindgen_22b6f19b3356e742

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i23@
foreign import ccall unsafe "hs_bindgen_4432bdff99ca3ab2" hs_bindgen_4432bdff99ca3ab2_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i23@
hs_bindgen_4432bdff99ca3ab2 :: IO (BG.Ptr BG.CInt)
hs_bindgen_4432bdff99ca3ab2 =
  BG.fromFFIType hs_bindgen_4432bdff99ca3ab2_base

{-# NOINLINE i23 #-}
{-| __C declaration:__ @i23@

    __defined at:__ @attributes\/visibility\/variables.h 45:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i23 :: BG.Ptr BG.CInt
i23 = BG.unsafePerformIO hs_bindgen_4432bdff99ca3ab2

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i24@
foreign import ccall unsafe "hs_bindgen_80552dd326e314dd" hs_bindgen_80552dd326e314dd_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i24@
hs_bindgen_80552dd326e314dd :: IO (BG.Ptr BG.CInt)
hs_bindgen_80552dd326e314dd =
  BG.fromFFIType hs_bindgen_80552dd326e314dd_base

{-# NOINLINE i24 #-}
{-| __C declaration:__ @i24@

    __defined at:__ @attributes\/visibility\/variables.h 46:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i24 :: BG.Ptr BG.CInt
i24 = BG.unsafePerformIO hs_bindgen_80552dd326e314dd

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i25@
foreign import ccall unsafe "hs_bindgen_e782a3b4c5c62b6d" hs_bindgen_e782a3b4c5c62b6d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i25@
hs_bindgen_e782a3b4c5c62b6d :: IO (BG.Ptr BG.CInt)
hs_bindgen_e782a3b4c5c62b6d =
  BG.fromFFIType hs_bindgen_e782a3b4c5c62b6d_base

{-# NOINLINE i25 #-}
{-| __C declaration:__ @i25@

    __defined at:__ @attributes\/visibility\/variables.h 49:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i25 :: BG.Ptr BG.CInt
i25 = BG.unsafePerformIO hs_bindgen_e782a3b4c5c62b6d

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i26@
foreign import ccall unsafe "hs_bindgen_9888ce2162201152" hs_bindgen_9888ce2162201152_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i26@
hs_bindgen_9888ce2162201152 :: IO (BG.Ptr BG.CInt)
hs_bindgen_9888ce2162201152 =
  BG.fromFFIType hs_bindgen_9888ce2162201152_base

{-# NOINLINE i26 #-}
{-| __C declaration:__ @i26@

    __defined at:__ @attributes\/visibility\/variables.h 50:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i26 :: BG.Ptr BG.CInt
i26 = BG.unsafePerformIO hs_bindgen_9888ce2162201152

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i27@
foreign import ccall unsafe "hs_bindgen_985cecf4c0c11aee" hs_bindgen_985cecf4c0c11aee_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i27@
hs_bindgen_985cecf4c0c11aee :: IO (BG.Ptr BG.CInt)
hs_bindgen_985cecf4c0c11aee =
  BG.fromFFIType hs_bindgen_985cecf4c0c11aee_base

{-# NOINLINE i27 #-}
{-| __C declaration:__ @i27@

    __defined at:__ @attributes\/visibility\/variables.h 51:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i27 :: BG.Ptr BG.CInt
i27 = BG.unsafePerformIO hs_bindgen_985cecf4c0c11aee

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i28@
foreign import ccall unsafe "hs_bindgen_b02407ada9cb4466" hs_bindgen_b02407ada9cb4466_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i28@
hs_bindgen_b02407ada9cb4466 :: IO (BG.Ptr BG.CInt)
hs_bindgen_b02407ada9cb4466 =
  BG.fromFFIType hs_bindgen_b02407ada9cb4466_base

{-# NOINLINE i28 #-}
{-| __C declaration:__ @i28@

    __defined at:__ @attributes\/visibility\/variables.h 52:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i28 :: BG.Ptr BG.CInt
i28 = BG.unsafePerformIO hs_bindgen_b02407ada9cb4466

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i29@
foreign import ccall unsafe "hs_bindgen_dc2295e107352a77" hs_bindgen_dc2295e107352a77_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_attributesvisibilityvariable_Example_get_i29@
hs_bindgen_dc2295e107352a77 :: IO (BG.Ptr BG.CInt)
hs_bindgen_dc2295e107352a77 =
  BG.fromFFIType hs_bindgen_dc2295e107352a77_base

{-# NOINLINE i29 #-}
{-| __C declaration:__ @i29@

    __defined at:__ @attributes\/visibility\/variables.h 53:55@

    __exported by:__ @attributes\/visibility\/variables.h@
-}
i29 :: BG.Ptr BG.CInt
i29 = BG.unsafePerformIO hs_bindgen_dc2295e107352a77
