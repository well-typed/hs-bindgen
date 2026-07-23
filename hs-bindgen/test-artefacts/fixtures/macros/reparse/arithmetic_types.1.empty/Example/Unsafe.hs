{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f1
    , Example.Unsafe.f2
    , Example.Unsafe.f3
    , Example.Unsafe.f4
    , Example.Unsafe.f5
    , Example.Unsafe.f6
    , Example.Unsafe.f7
    , Example.Unsafe.f8
    , Example.Unsafe.f9
    , Example.Unsafe.f10
    , Example.Unsafe.f11
    , Example.Unsafe.f12
    , Example.Unsafe.f13
    , Example.Unsafe.f14
    , Example.Unsafe.f15
    , Example.Unsafe.f16
    , Example.Unsafe.f17
    , Example.Unsafe.f18
    , Example.Unsafe.f19
    , Example.Unsafe.f20
    , Example.Unsafe.f21
    , Example.Unsafe.f22
    , Example.Unsafe.f23
    , Example.Unsafe.f24
    , Example.Unsafe.f25
    , Example.Unsafe.f26
    , Example.Unsafe.f27
    , Example.Unsafe.f28
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/arithmetic_types.h>"
  , "signed int hs_bindgen_d6e3d8093d108780 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return (f1)(arg1);"
  , "}"
  , "signed int hs_bindgen_dc34e6197f6f39df ("
  , "  signed char arg1"
  , ")"
  , "{"
  , "  return (f2)(arg1);"
  , "}"
  , "signed int hs_bindgen_b642c5b44d663af5 ("
  , "  unsigned char arg1"
  , ")"
  , "{"
  , "  return (f3)(arg1);"
  , "}"
  , "signed int hs_bindgen_27f5c806bf4593e7 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f4)(arg1);"
  , "}"
  , "signed int hs_bindgen_e8c0eab09a235501 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f5)(arg1);"
  , "}"
  , "signed int hs_bindgen_7925bdb53e040792 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f6)(arg1);"
  , "}"
  , "signed int hs_bindgen_e6eb4efed37c1708 ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (f7)(arg1);"
  , "}"
  , "signed int hs_bindgen_21cc8ace7d7441bd ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f8)(arg1);"
  , "}"
  , "signed int hs_bindgen_ddca0323ddaaee35 ("
  , "  unsigned short arg1"
  , ")"
  , "{"
  , "  return (f9)(arg1);"
  , "}"
  , "signed int hs_bindgen_d42a7635d5a1a314 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f10)(arg1);"
  , "}"
  , "signed int hs_bindgen_111c67845229b054 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f11)(arg1);"
  , "}"
  , "signed int hs_bindgen_6c05305fb77f1073 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f12)(arg1);"
  , "}"
  , "signed int hs_bindgen_661a8e9409698789 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f13)(arg1);"
  , "}"
  , "signed int hs_bindgen_3bdf905c84cb8fb9 ("
  , "  unsigned int arg1"
  , ")"
  , "{"
  , "  return (f14)(arg1);"
  , "}"
  , "signed int hs_bindgen_212883c913884d27 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f15)(arg1);"
  , "}"
  , "signed int hs_bindgen_f72cafc33ec81360 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f16)(arg1);"
  , "}"
  , "signed int hs_bindgen_ea5f62444673cf16 ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f17)(arg1);"
  , "}"
  , "signed int hs_bindgen_9f4e020f892b30cb ("
  , "  signed long arg1"
  , ")"
  , "{"
  , "  return (f18)(arg1);"
  , "}"
  , "signed int hs_bindgen_47f005495bf88291 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f19)(arg1);"
  , "}"
  , "signed int hs_bindgen_d02703a7ce8fade2 ("
  , "  unsigned long arg1"
  , ")"
  , "{"
  , "  return (f20)(arg1);"
  , "}"
  , "signed int hs_bindgen_9f98e2aafc8c5a95 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f21)(arg1);"
  , "}"
  , "signed int hs_bindgen_430da5f47172739a ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f22)(arg1);"
  , "}"
  , "signed int hs_bindgen_bd83aa03a61a8a06 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f23)(arg1);"
  , "}"
  , "signed int hs_bindgen_affcee7ffeebbac9 ("
  , "  signed long long arg1"
  , ")"
  , "{"
  , "  return (f24)(arg1);"
  , "}"
  , "signed int hs_bindgen_b8a7fb0f9add23ab ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return (f25)(arg1);"
  , "}"
  , "signed int hs_bindgen_377c3e6bd5b69285 ("
  , "  unsigned long long arg1"
  , ")"
  , "{"
  , "  return (f26)(arg1);"
  , "}"
  , "signed int hs_bindgen_b82bda1eaf8b02ba ("
  , "  float arg1"
  , ")"
  , "{"
  , "  return (f27)(arg1);"
  , "}"
  , "signed int hs_bindgen_8d36f32587832dae ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (f28)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_d6e3d8093d108780" hs_bindgen_d6e3d8093d108780_base ::
     BG.Int8
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f1@
hs_bindgen_d6e3d8093d108780 ::
     BG.CChar
  -> IO BG.CInt
hs_bindgen_d6e3d8093d108780 =
  BG.fromFFIType hs_bindgen_d6e3d8093d108780_base

{-| __C declaration:__ @f1@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 21:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f1 ::
     BG.CChar
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f1 = hs_bindgen_d6e3d8093d108780

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_dc34e6197f6f39df" hs_bindgen_dc34e6197f6f39df_base ::
     BG.Int8
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f2@
hs_bindgen_dc34e6197f6f39df ::
     BG.CSChar
  -> IO BG.CInt
hs_bindgen_dc34e6197f6f39df =
  BG.fromFFIType hs_bindgen_dc34e6197f6f39df_base

{-| __C declaration:__ @f2@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 22:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f2 ::
     BG.CSChar
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f2 = hs_bindgen_dc34e6197f6f39df

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_b642c5b44d663af5" hs_bindgen_b642c5b44d663af5_base ::
     BG.Word8
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f3@
hs_bindgen_b642c5b44d663af5 ::
     BG.CUChar
  -> IO BG.CInt
hs_bindgen_b642c5b44d663af5 =
  BG.fromFFIType hs_bindgen_b642c5b44d663af5_base

{-| __C declaration:__ @f3@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 23:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f3 ::
     BG.CUChar
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f3 = hs_bindgen_b642c5b44d663af5

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f4@
foreign import ccall unsafe "hs_bindgen_27f5c806bf4593e7" hs_bindgen_27f5c806bf4593e7_base ::
     BG.Int16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f4@
hs_bindgen_27f5c806bf4593e7 ::
     BG.CShort
  -> IO BG.CInt
hs_bindgen_27f5c806bf4593e7 =
  BG.fromFFIType hs_bindgen_27f5c806bf4593e7_base

{-| __C declaration:__ @f4@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 28:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f4 ::
     BG.CShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f4 = hs_bindgen_27f5c806bf4593e7

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f5@
foreign import ccall unsafe "hs_bindgen_e8c0eab09a235501" hs_bindgen_e8c0eab09a235501_base ::
     BG.Int16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f5@
hs_bindgen_e8c0eab09a235501 ::
     BG.CShort
  -> IO BG.CInt
hs_bindgen_e8c0eab09a235501 =
  BG.fromFFIType hs_bindgen_e8c0eab09a235501_base

{-| __C declaration:__ @f5@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 29:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f5 ::
     BG.CShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f5 = hs_bindgen_e8c0eab09a235501

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f6@
foreign import ccall unsafe "hs_bindgen_7925bdb53e040792" hs_bindgen_7925bdb53e040792_base ::
     BG.Int16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f6@
hs_bindgen_7925bdb53e040792 ::
     BG.CShort
  -> IO BG.CInt
hs_bindgen_7925bdb53e040792 =
  BG.fromFFIType hs_bindgen_7925bdb53e040792_base

{-| __C declaration:__ @f6@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 30:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f6 ::
     BG.CShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f6 = hs_bindgen_7925bdb53e040792

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f7@
foreign import ccall unsafe "hs_bindgen_e6eb4efed37c1708" hs_bindgen_e6eb4efed37c1708_base ::
     BG.Int16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f7@
hs_bindgen_e6eb4efed37c1708 ::
     BG.CShort
  -> IO BG.CInt
hs_bindgen_e6eb4efed37c1708 =
  BG.fromFFIType hs_bindgen_e6eb4efed37c1708_base

{-| __C declaration:__ @f7@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 31:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f7 ::
     BG.CShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f7 = hs_bindgen_e6eb4efed37c1708

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f8@
foreign import ccall unsafe "hs_bindgen_21cc8ace7d7441bd" hs_bindgen_21cc8ace7d7441bd_base ::
     BG.Word16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f8@
hs_bindgen_21cc8ace7d7441bd ::
     BG.CUShort
  -> IO BG.CInt
hs_bindgen_21cc8ace7d7441bd =
  BG.fromFFIType hs_bindgen_21cc8ace7d7441bd_base

{-| __C declaration:__ @f8@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 32:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f8 ::
     BG.CUShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f8 = hs_bindgen_21cc8ace7d7441bd

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f9@
foreign import ccall unsafe "hs_bindgen_ddca0323ddaaee35" hs_bindgen_ddca0323ddaaee35_base ::
     BG.Word16
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f9@
hs_bindgen_ddca0323ddaaee35 ::
     BG.CUShort
  -> IO BG.CInt
hs_bindgen_ddca0323ddaaee35 =
  BG.fromFFIType hs_bindgen_ddca0323ddaaee35_base

{-| __C declaration:__ @f9@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 33:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f9 ::
     BG.CUShort
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f9 = hs_bindgen_ddca0323ddaaee35

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f10@
foreign import ccall unsafe "hs_bindgen_d42a7635d5a1a314" hs_bindgen_d42a7635d5a1a314_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f10@
hs_bindgen_d42a7635d5a1a314 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_d42a7635d5a1a314 =
  BG.fromFFIType hs_bindgen_d42a7635d5a1a314_base

{-| __C declaration:__ @f10@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 35:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f10 ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f10 = hs_bindgen_d42a7635d5a1a314

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f11@
foreign import ccall unsafe "hs_bindgen_111c67845229b054" hs_bindgen_111c67845229b054_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f11@
hs_bindgen_111c67845229b054 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_111c67845229b054 =
  BG.fromFFIType hs_bindgen_111c67845229b054_base

{-| __C declaration:__ @f11@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 36:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f11 ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f11 = hs_bindgen_111c67845229b054

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f12@
foreign import ccall unsafe "hs_bindgen_6c05305fb77f1073" hs_bindgen_6c05305fb77f1073_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f12@
hs_bindgen_6c05305fb77f1073 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_6c05305fb77f1073 =
  BG.fromFFIType hs_bindgen_6c05305fb77f1073_base

{-| __C declaration:__ @f12@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 37:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f12 ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f12 = hs_bindgen_6c05305fb77f1073

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f13@
foreign import ccall unsafe "hs_bindgen_661a8e9409698789" hs_bindgen_661a8e9409698789_base ::
     BG.Word32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f13@
hs_bindgen_661a8e9409698789 ::
     BG.CUInt
  -> IO BG.CInt
hs_bindgen_661a8e9409698789 =
  BG.fromFFIType hs_bindgen_661a8e9409698789_base

{-| __C declaration:__ @f13@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 38:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f13 ::
     BG.CUInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f13 = hs_bindgen_661a8e9409698789

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f14@
foreign import ccall unsafe "hs_bindgen_3bdf905c84cb8fb9" hs_bindgen_3bdf905c84cb8fb9_base ::
     BG.Word32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f14@
hs_bindgen_3bdf905c84cb8fb9 ::
     BG.CUInt
  -> IO BG.CInt
hs_bindgen_3bdf905c84cb8fb9 =
  BG.fromFFIType hs_bindgen_3bdf905c84cb8fb9_base

{-| __C declaration:__ @f14@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 39:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f14 ::
     BG.CUInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f14 = hs_bindgen_3bdf905c84cb8fb9

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f15@
foreign import ccall unsafe "hs_bindgen_212883c913884d27" hs_bindgen_212883c913884d27_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f15@
hs_bindgen_212883c913884d27 ::
     BG.CLong
  -> IO BG.CInt
hs_bindgen_212883c913884d27 =
  BG.fromFFIType hs_bindgen_212883c913884d27_base

{-| __C declaration:__ @f15@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 41:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f15 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f15 = hs_bindgen_212883c913884d27

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f16@
foreign import ccall unsafe "hs_bindgen_f72cafc33ec81360" hs_bindgen_f72cafc33ec81360_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f16@
hs_bindgen_f72cafc33ec81360 ::
     BG.CLong
  -> IO BG.CInt
hs_bindgen_f72cafc33ec81360 =
  BG.fromFFIType hs_bindgen_f72cafc33ec81360_base

{-| __C declaration:__ @f16@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 42:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f16 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f16 = hs_bindgen_f72cafc33ec81360

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f17@
foreign import ccall unsafe "hs_bindgen_ea5f62444673cf16" hs_bindgen_ea5f62444673cf16_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f17@
hs_bindgen_ea5f62444673cf16 ::
     BG.CLong
  -> IO BG.CInt
hs_bindgen_ea5f62444673cf16 =
  BG.fromFFIType hs_bindgen_ea5f62444673cf16_base

{-| __C declaration:__ @f17@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 43:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f17 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f17 = hs_bindgen_ea5f62444673cf16

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f18@
foreign import ccall unsafe "hs_bindgen_9f4e020f892b30cb" hs_bindgen_9f4e020f892b30cb_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f18@
hs_bindgen_9f4e020f892b30cb ::
     BG.CLong
  -> IO BG.CInt
hs_bindgen_9f4e020f892b30cb =
  BG.fromFFIType hs_bindgen_9f4e020f892b30cb_base

{-| __C declaration:__ @f18@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 44:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f18 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f18 = hs_bindgen_9f4e020f892b30cb

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f19@
foreign import ccall unsafe "hs_bindgen_47f005495bf88291" hs_bindgen_47f005495bf88291_base ::
     BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f19@
hs_bindgen_47f005495bf88291 ::
     BG.CULong
  -> IO BG.CInt
hs_bindgen_47f005495bf88291 =
  BG.fromFFIType hs_bindgen_47f005495bf88291_base

{-| __C declaration:__ @f19@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 45:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f19 ::
     BG.CULong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f19 = hs_bindgen_47f005495bf88291

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f20@
foreign import ccall unsafe "hs_bindgen_d02703a7ce8fade2" hs_bindgen_d02703a7ce8fade2_base ::
     BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f20@
hs_bindgen_d02703a7ce8fade2 ::
     BG.CULong
  -> IO BG.CInt
hs_bindgen_d02703a7ce8fade2 =
  BG.fromFFIType hs_bindgen_d02703a7ce8fade2_base

{-| __C declaration:__ @f20@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 46:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f20 ::
     BG.CULong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f20 = hs_bindgen_d02703a7ce8fade2

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f21@
foreign import ccall unsafe "hs_bindgen_9f98e2aafc8c5a95" hs_bindgen_9f98e2aafc8c5a95_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f21@
hs_bindgen_9f98e2aafc8c5a95 ::
     BG.CLLong
  -> IO BG.CInt
hs_bindgen_9f98e2aafc8c5a95 =
  BG.fromFFIType hs_bindgen_9f98e2aafc8c5a95_base

{-| __C declaration:__ @f21@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 48:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f21 ::
     BG.CLLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f21 = hs_bindgen_9f98e2aafc8c5a95

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f22@
foreign import ccall unsafe "hs_bindgen_430da5f47172739a" hs_bindgen_430da5f47172739a_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f22@
hs_bindgen_430da5f47172739a ::
     BG.CLLong
  -> IO BG.CInt
hs_bindgen_430da5f47172739a =
  BG.fromFFIType hs_bindgen_430da5f47172739a_base

{-| __C declaration:__ @f22@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 49:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f22 ::
     BG.CLLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f22 = hs_bindgen_430da5f47172739a

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f23@
foreign import ccall unsafe "hs_bindgen_bd83aa03a61a8a06" hs_bindgen_bd83aa03a61a8a06_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f23@
hs_bindgen_bd83aa03a61a8a06 ::
     BG.CLLong
  -> IO BG.CInt
hs_bindgen_bd83aa03a61a8a06 =
  BG.fromFFIType hs_bindgen_bd83aa03a61a8a06_base

{-| __C declaration:__ @f23@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 50:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f23 ::
     BG.CLLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f23 = hs_bindgen_bd83aa03a61a8a06

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f24@
foreign import ccall unsafe "hs_bindgen_affcee7ffeebbac9" hs_bindgen_affcee7ffeebbac9_base ::
     BG.Int64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f24@
hs_bindgen_affcee7ffeebbac9 ::
     BG.CLLong
  -> IO BG.CInt
hs_bindgen_affcee7ffeebbac9 =
  BG.fromFFIType hs_bindgen_affcee7ffeebbac9_base

{-| __C declaration:__ @f24@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 51:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f24 ::
     BG.CLLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f24 = hs_bindgen_affcee7ffeebbac9

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f25@
foreign import ccall unsafe "hs_bindgen_b8a7fb0f9add23ab" hs_bindgen_b8a7fb0f9add23ab_base ::
     BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f25@
hs_bindgen_b8a7fb0f9add23ab ::
     BG.CULLong
  -> IO BG.CInt
hs_bindgen_b8a7fb0f9add23ab =
  BG.fromFFIType hs_bindgen_b8a7fb0f9add23ab_base

{-| __C declaration:__ @f25@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 52:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f25 ::
     BG.CULLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f25 = hs_bindgen_b8a7fb0f9add23ab

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f26@
foreign import ccall unsafe "hs_bindgen_377c3e6bd5b69285" hs_bindgen_377c3e6bd5b69285_base ::
     BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f26@
hs_bindgen_377c3e6bd5b69285 ::
     BG.CULLong
  -> IO BG.CInt
hs_bindgen_377c3e6bd5b69285 =
  BG.fromFFIType hs_bindgen_377c3e6bd5b69285_base

{-| __C declaration:__ @f26@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 53:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f26 ::
     BG.CULLong
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f26 = hs_bindgen_377c3e6bd5b69285

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f27@
foreign import ccall unsafe "hs_bindgen_b82bda1eaf8b02ba" hs_bindgen_b82bda1eaf8b02ba_base ::
     Float
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f27@
hs_bindgen_b82bda1eaf8b02ba ::
     BG.CFloat
  -> IO BG.CInt
hs_bindgen_b82bda1eaf8b02ba =
  BG.fromFFIType hs_bindgen_b82bda1eaf8b02ba_base

{-| __C declaration:__ @f27@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 58:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f27 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f27 = hs_bindgen_b82bda1eaf8b02ba

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f28@
foreign import ccall unsafe "hs_bindgen_8d36f32587832dae" hs_bindgen_8d36f32587832dae_base ::
     Double
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsearithmetic_type_Example_Unsafe_f28@
hs_bindgen_8d36f32587832dae ::
     BG.CDouble
  -> IO BG.CInt
hs_bindgen_8d36f32587832dae =
  BG.fromFFIType hs_bindgen_8d36f32587832dae_base

{-| __C declaration:__ @f28@

    __defined at:__ @macros\/reparse\/arithmetic_types.h 59:3@

    __exported by:__ @macros\/reparse\/arithmetic_types.h@
-}
f28 ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
f28 = hs_bindgen_8d36f32587832dae
