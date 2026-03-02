{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.my_array
    , Example.Global.ordinary_float
    , Example.Global.ordinary_double
    , Example.Global.ordinary_char
    , Example.Global.signed_char
    , Example.Global.unsigned_char
    , Example.Global.ordinary_signed_short
    , Example.Global.explicit_signed_short
    , Example.Global.unsigned_short
    , Example.Global.ordinary_signed_int
    , Example.Global.explicit_signed_int
    , Example.Global.unsigned_int
    , Example.Global.ordinary_signed_long
    , Example.Global.explicit_signed_long
    , Example.Global.unsigned_long
    , Example.Global.ordinary_signed_long_long
    , Example.Global.explicit_signed_long_long
    , Example.Global.unsigned_long_long
    , Example.Global.ordinary_void_pointer
    , Example.Global.ordinary_float_pointer
    , Example.Global.ordinary_double_pointer
    , Example.Global.ordinary_char_pointer
    , Example.Global.signed_char_pointer
    , Example.Global.unsigned_char_pointer
    , Example.Global.ordinary_signed_short_pointer
    , Example.Global.explicit_signed_short_pointer
    , Example.Global.unsigned_short_pointer
    , Example.Global.ordinary_signed_int_pointer
    , Example.Global.explicit_signed_int_pointer
    , Example.Global.unsigned_int_pointer
    , Example.Global.ordinary_signed_long_pointer
    , Example.Global.explicit_signed_long_pointer
    , Example.Global.unsigned_long_pointer
    , Example.Global.ordinary_signed_long_long_pointer
    , Example.Global.explicit_signed_long_long_pointer
    , Example.Global.unsigned_long_long_pointer
    , Example.Global.ordinary_float_array
    , Example.Global.ordinary_double_array
    , Example.Global.ordinary_signed_char_array
    , Example.Global.explicit_signed_char_array
    , Example.Global.unsigned_char_array
    , Example.Global.ordinary_signed_short_array
    , Example.Global.explicit_signed_short_array
    , Example.Global.unsigned_short_array
    , Example.Global.ordinary_signed_int_array
    , Example.Global.explicit_signed_int_array
    , Example.Global.unsigned_int_array
    , Example.Global.ordinary_signed_long_array
    , Example.Global.explicit_signed_long_array
    , Example.Global.unsigned_long_array
    , Example.Global.ordinary_signed_long_long_array
    , Example.Global.explicit_signed_long_long_array
    , Example.Global.unsigned_long_long_array
    , Example.Global.ordinary_void_pointer_array
    , Example.Global.ordinary_float_pointer_array
    , Example.Global.ordinary_double_pointer_array
    , Example.Global.ordinary_signed_char_pointer_array
    , Example.Global.explicit_signed_char_pointer_array
    , Example.Global.unsigned_char_pointer_array
    , Example.Global.ordinary_signed_short_pointer_array
    , Example.Global.explicit_signed_short_pointer_array
    , Example.Global.unsigned_short_pointer_array
    , Example.Global.ordinary_signed_int_pointer_array
    , Example.Global.explicit_signed_int_pointer_array
    , Example.Global.unsigned_int_pointer_array
    , Example.Global.ordinary_signed_long_pointer_array
    , Example.Global.explicit_signed_long_pointer_array
    , Example.Global.unsigned_long_pointer_array
    , Example.Global.ordinary_signed_long_long_pointer_array
    , Example.Global.explicit_signed_long_long_pointer_array
    , Example.Global.unsigned_long_long_pointer_array
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <comprehensive/c2hsc.h>"
  , "/* test_comprehensivec2hsc_Example_get_my_array */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_b2862a117942ba8c (void))[]) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &my_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_float */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_fbe24203b7174cce (void)"
  , "{"
  , "  return &ordinary_float;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_double */"
  , "__attribute__ ((const))"
  , "double *hs_bindgen_c996ca409147b439 (void)"
  , "{"
  , "  return &ordinary_double;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_char */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_b5c47cbce17ba18f (void)"
  , "{"
  , "  return &ordinary_char;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_signed_char */"
  , "__attribute__ ((const))"
  , "signed char *hs_bindgen_74214b0c0c8011ef (void)"
  , "{"
  , "  return &signed_char;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_char */"
  , "__attribute__ ((const))"
  , "unsigned char *hs_bindgen_074079404a90186b (void)"
  , "{"
  , "  return &unsigned_char;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_short */"
  , "__attribute__ ((const))"
  , "signed short *hs_bindgen_e432ab522350f84b (void)"
  , "{"
  , "  return &ordinary_signed_short;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_short */"
  , "__attribute__ ((const))"
  , "signed short *hs_bindgen_17fa3df7c6690161 (void)"
  , "{"
  , "  return &explicit_signed_short;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_short */"
  , "__attribute__ ((const))"
  , "unsigned short *hs_bindgen_20f280e98d58aacc (void)"
  , "{"
  , "  return &unsigned_short;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_int */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_78b4f07c60ef3ef7 (void)"
  , "{"
  , "  return &ordinary_signed_int;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_int */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_41643d3737e9213c (void)"
  , "{"
  , "  return &explicit_signed_int;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_int */"
  , "__attribute__ ((const))"
  , "unsigned int *hs_bindgen_6e686e175c956cb8 (void)"
  , "{"
  , "  return &unsigned_int;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long */"
  , "__attribute__ ((const))"
  , "signed long *hs_bindgen_6a41c38f7fbb41d9 (void)"
  , "{"
  , "  return &ordinary_signed_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long */"
  , "__attribute__ ((const))"
  , "signed long *hs_bindgen_a3b5191325da8230 (void)"
  , "{"
  , "  return &explicit_signed_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long */"
  , "__attribute__ ((const))"
  , "unsigned long *hs_bindgen_df563b6d35ec4e8f (void)"
  , "{"
  , "  return &unsigned_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_long */"
  , "__attribute__ ((const))"
  , "signed long long *hs_bindgen_59c6159f8798505a (void)"
  , "{"
  , "  return &ordinary_signed_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_long */"
  , "__attribute__ ((const))"
  , "signed long long *hs_bindgen_bfd63280c68460d2 (void)"
  , "{"
  , "  return &explicit_signed_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_long */"
  , "__attribute__ ((const))"
  , "unsigned long long *hs_bindgen_8654be9a041c2116 (void)"
  , "{"
  , "  return &unsigned_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_void_pointer */"
  , "__attribute__ ((const))"
  , "void **hs_bindgen_d6cafaa0834895a2 (void)"
  , "{"
  , "  return &ordinary_void_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_float_pointer */"
  , "__attribute__ ((const))"
  , "float **hs_bindgen_35ef3ffa1a5167f7 (void)"
  , "{"
  , "  return &ordinary_float_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_double_pointer */"
  , "__attribute__ ((const))"
  , "double **hs_bindgen_5e831159f7556399 (void)"
  , "{"
  , "  return &ordinary_double_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_char_pointer */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_08236e44cabe67f4 (void)"
  , "{"
  , "  return &ordinary_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_signed_char_pointer */"
  , "__attribute__ ((const))"
  , "signed char **hs_bindgen_66d08eddc2fecb35 (void)"
  , "{"
  , "  return &signed_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_char_pointer */"
  , "__attribute__ ((const))"
  , "unsigned char **hs_bindgen_5c76c506cb4fd78e (void)"
  , "{"
  , "  return &unsigned_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer */"
  , "__attribute__ ((const))"
  , "signed short **hs_bindgen_cd114d033253fe37 (void)"
  , "{"
  , "  return &ordinary_signed_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer */"
  , "__attribute__ ((const))"
  , "signed short **hs_bindgen_49ffa4c56ce34e2f (void)"
  , "{"
  , "  return &explicit_signed_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_short_pointer */"
  , "__attribute__ ((const))"
  , "unsigned short **hs_bindgen_fb80ad6945c7151d (void)"
  , "{"
  , "  return &unsigned_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_dfef04ad64871bf8 (void)"
  , "{"
  , "  return &ordinary_signed_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_515d5232453c4285 (void)"
  , "{"
  , "  return &explicit_signed_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_int_pointer */"
  , "__attribute__ ((const))"
  , "unsigned int **hs_bindgen_9f2724dae8d3b3fa (void)"
  , "{"
  , "  return &unsigned_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long **hs_bindgen_a6b4a786a6644424 (void)"
  , "{"
  , "  return &ordinary_signed_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long **hs_bindgen_4df40729ac557753 (void)"
  , "{"
  , "  return &explicit_signed_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_pointer */"
  , "__attribute__ ((const))"
  , "unsigned long **hs_bindgen_30379268a6bf43b0 (void)"
  , "{"
  , "  return &unsigned_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long long **hs_bindgen_63dedd7015d9ed47 (void)"
  , "{"
  , "  return &ordinary_signed_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long long **hs_bindgen_9f47ed329f79e647 (void)"
  , "{"
  , "  return &explicit_signed_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer */"
  , "__attribute__ ((const))"
  , "unsigned long long **hs_bindgen_7d2dd0a78dde4b41 (void)"
  , "{"
  , "  return &unsigned_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_float_array */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_dbf314f660deda12 (void))[10]"
  , "{"
  , "  return &ordinary_float_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_double_array */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_d554c83416b4ffaa (void))[10]"
  , "{"
  , "  return &ordinary_double_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_char_array */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_9a57c94ff3fad985 (void))[10]"
  , "{"
  , "  return &ordinary_signed_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_char_array */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_335217e9e2c8fe6a (void))[10]"
  , "{"
  , "  return &explicit_signed_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_char_array */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_1e72daddbfe2b7ac (void))[10]"
  , "{"
  , "  return &unsigned_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_short_array */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_109462175baa2bdf (void))[10]"
  , "{"
  , "  return &ordinary_signed_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_short_array */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_50f788c18928793d (void))[10]"
  , "{"
  , "  return &explicit_signed_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_short_array */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_d5ccc3314ab2eb42 (void))[10]"
  , "{"
  , "  return &unsigned_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_int_array */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f7824c4ee10b0b90 (void))[10]"
  , "{"
  , "  return &ordinary_signed_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_int_array */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_448c7a47209c8b7a (void))[10]"
  , "{"
  , "  return &explicit_signed_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_int_array */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_66499a8ae9382b9e (void))[10]"
  , "{"
  , "  return &unsigned_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_array */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_896d3ba25d886356 (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_array */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_c4ec5e2d6cf5b5ed (void))[10]"
  , "{"
  , "  return &explicit_signed_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_array */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_e3fb79df55414b09 (void))[10]"
  , "{"
  , "  return &unsigned_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_array */"
  , "__attribute__ ((const))"
  , "signed long long (*hs_bindgen_02c15bdcdfc54945 (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_long_array */"
  , "__attribute__ ((const))"
  , "signed long long (*hs_bindgen_4c934e5832898551 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_long_array */"
  , "__attribute__ ((const))"
  , "unsigned long long (*hs_bindgen_af47cf5cc0016405 (void))[10]"
  , "{"
  , "  return &unsigned_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_void_pointer_array */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_b0da135021f97e27 (void))[10]"
  , "{"
  , "  return &ordinary_void_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_float_pointer_array */"
  , "__attribute__ ((const))"
  , "float *(*hs_bindgen_41ce3a49f177f5a8 (void))[10]"
  , "{"
  , "  return &ordinary_float_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_double_pointer_array */"
  , "__attribute__ ((const))"
  , "double *(*hs_bindgen_cfe3890477df51c8 (void))[10]"
  , "{"
  , "  return &ordinary_double_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_char_pointer_array */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_e92be5e29785f22b (void))[10]"
  , "{"
  , "  return &ordinary_signed_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_char_pointer_array */"
  , "__attribute__ ((const))"
  , "signed char *(*hs_bindgen_c8caf99249e8ad58 (void))[10]"
  , "{"
  , "  return &explicit_signed_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_char_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned char *(*hs_bindgen_7028963176c7a21d (void))[10]"
  , "{"
  , "  return &unsigned_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer_array */"
  , "__attribute__ ((const))"
  , "signed short *(*hs_bindgen_1866849231d566a0 (void))[10]"
  , "{"
  , "  return &ordinary_signed_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer_array */"
  , "__attribute__ ((const))"
  , "signed short *(*hs_bindgen_68ed6d444e1b6011 (void))[10]"
  , "{"
  , "  return &explicit_signed_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_short_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned short *(*hs_bindgen_7c0ca48c3269649a (void))[10]"
  , "{"
  , "  return &unsigned_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer_array */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_58e6503e8ccbeb70 (void))[10]"
  , "{"
  , "  return &ordinary_signed_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer_array */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_21b8395de873bc0a (void))[10]"
  , "{"
  , "  return &explicit_signed_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_int_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned int *(*hs_bindgen_667d31d5b7b5106e (void))[10]"
  , "{"
  , "  return &unsigned_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long *(*hs_bindgen_eb34bb7d213dcd3d (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long *(*hs_bindgen_3386ff9d0e54545d (void))[10]"
  , "{"
  , "  return &explicit_signed_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned long *(*hs_bindgen_510be86496a28c91 (void))[10]"
  , "{"
  , "  return &unsigned_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long long *(*hs_bindgen_25f60aa140d341a7 (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long long *(*hs_bindgen_aced8afe33f53905 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned long long *(*hs_bindgen_9534a7c84390edc4 (void))[10]"
  , "{"
  , "  return &unsigned_long_long_pointer_array;"
  , "}"
  ]))

-- __unique:__ @test_comprehensivec2hsc_Example_get_my_array@
foreign import ccall unsafe "hs_bindgen_b2862a117942ba8c" hs_bindgen_b2862a117942ba8c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_my_array@
hs_bindgen_b2862a117942ba8c :: IO (RIP.Ptr (IA.IncompleteArray (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))))
hs_bindgen_b2862a117942ba8c =
  RIP.fromFFIType hs_bindgen_b2862a117942ba8c_base

{-# NOINLINE my_array #-}
{-| __C declaration:__ @my_array@

    __defined at:__ @comprehensive\/c2hsc.h 26:14@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
my_array :: RIP.Ptr (IA.IncompleteArray (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)))
my_array =
  RIP.unsafePerformIO hs_bindgen_b2862a117942ba8c

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float@
foreign import ccall unsafe "hs_bindgen_fbe24203b7174cce" hs_bindgen_fbe24203b7174cce_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float@
hs_bindgen_fbe24203b7174cce :: IO (RIP.Ptr RIP.CFloat)
hs_bindgen_fbe24203b7174cce =
  RIP.fromFFIType hs_bindgen_fbe24203b7174cce_base

{-# NOINLINE ordinary_float #-}
{-| Primitive types

  NOTE: Here and elsewhere the original test suite has some references to `long double`, some commented out, and some---confusingly---not, with an expectation of a translation to `CDouble`; the latter I think is a bug in the original test suite. Here we leave all of these test cases in; we emit an "unsupported" message for these and omit the declaration in the generated bindings.

__C declaration:__ @ordinary_float@

__defined at:__ @comprehensive\/c2hsc.h 88:20@

__exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float :: RIP.Ptr RIP.CFloat
ordinary_float =
  RIP.unsafePerformIO hs_bindgen_fbe24203b7174cce

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double@
foreign import ccall unsafe "hs_bindgen_c996ca409147b439" hs_bindgen_c996ca409147b439_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double@
hs_bindgen_c996ca409147b439 :: IO (RIP.Ptr RIP.CDouble)
hs_bindgen_c996ca409147b439 =
  RIP.fromFFIType hs_bindgen_c996ca409147b439_base

{-# NOINLINE ordinary_double #-}
{-| __C declaration:__ @ordinary_double@

    __defined at:__ @comprehensive\/c2hsc.h 89:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double :: RIP.Ptr RIP.CDouble
ordinary_double =
  RIP.unsafePerformIO hs_bindgen_c996ca409147b439

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_char@
foreign import ccall unsafe "hs_bindgen_b5c47cbce17ba18f" hs_bindgen_b5c47cbce17ba18f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_char@
hs_bindgen_b5c47cbce17ba18f :: IO (RIP.Ptr RIP.CChar)
hs_bindgen_b5c47cbce17ba18f =
  RIP.fromFFIType hs_bindgen_b5c47cbce17ba18f_base

{-# NOINLINE ordinary_char #-}
{-| __C declaration:__ @ordinary_char@

    __defined at:__ @comprehensive\/c2hsc.h 92:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_char :: RIP.Ptr RIP.CChar
ordinary_char =
  RIP.unsafePerformIO hs_bindgen_b5c47cbce17ba18f

-- __unique:__ @test_comprehensivec2hsc_Example_get_signed_char@
foreign import ccall unsafe "hs_bindgen_74214b0c0c8011ef" hs_bindgen_74214b0c0c8011ef_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_signed_char@
hs_bindgen_74214b0c0c8011ef :: IO (RIP.Ptr RIP.CSChar)
hs_bindgen_74214b0c0c8011ef =
  RIP.fromFFIType hs_bindgen_74214b0c0c8011ef_base

{-# NOINLINE signed_char #-}
{-| __C declaration:__ @signed_char@

    __defined at:__ @comprehensive\/c2hsc.h 93:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
signed_char :: RIP.Ptr RIP.CSChar
signed_char =
  RIP.unsafePerformIO hs_bindgen_74214b0c0c8011ef

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char@
foreign import ccall unsafe "hs_bindgen_074079404a90186b" hs_bindgen_074079404a90186b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char@
hs_bindgen_074079404a90186b :: IO (RIP.Ptr RIP.CUChar)
hs_bindgen_074079404a90186b =
  RIP.fromFFIType hs_bindgen_074079404a90186b_base

{-# NOINLINE unsigned_char #-}
{-| __C declaration:__ @unsigned_char@

    __defined at:__ @comprehensive\/c2hsc.h 94:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char :: RIP.Ptr RIP.CUChar
unsigned_char =
  RIP.unsafePerformIO hs_bindgen_074079404a90186b

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short@
foreign import ccall unsafe "hs_bindgen_e432ab522350f84b" hs_bindgen_e432ab522350f84b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short@
hs_bindgen_e432ab522350f84b :: IO (RIP.Ptr RIP.CShort)
hs_bindgen_e432ab522350f84b =
  RIP.fromFFIType hs_bindgen_e432ab522350f84b_base

{-# NOINLINE ordinary_signed_short #-}
{-| __C declaration:__ @ordinary_signed_short@

    __defined at:__ @comprehensive\/c2hsc.h 96:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short :: RIP.Ptr RIP.CShort
ordinary_signed_short =
  RIP.unsafePerformIO hs_bindgen_e432ab522350f84b

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short@
foreign import ccall unsafe "hs_bindgen_17fa3df7c6690161" hs_bindgen_17fa3df7c6690161_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short@
hs_bindgen_17fa3df7c6690161 :: IO (RIP.Ptr RIP.CShort)
hs_bindgen_17fa3df7c6690161 =
  RIP.fromFFIType hs_bindgen_17fa3df7c6690161_base

{-# NOINLINE explicit_signed_short #-}
{-| __C declaration:__ @explicit_signed_short@

    __defined at:__ @comprehensive\/c2hsc.h 97:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short :: RIP.Ptr RIP.CShort
explicit_signed_short =
  RIP.unsafePerformIO hs_bindgen_17fa3df7c6690161

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short@
foreign import ccall unsafe "hs_bindgen_20f280e98d58aacc" hs_bindgen_20f280e98d58aacc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short@
hs_bindgen_20f280e98d58aacc :: IO (RIP.Ptr RIP.CUShort)
hs_bindgen_20f280e98d58aacc =
  RIP.fromFFIType hs_bindgen_20f280e98d58aacc_base

{-# NOINLINE unsigned_short #-}
{-| __C declaration:__ @unsigned_short@

    __defined at:__ @comprehensive\/c2hsc.h 98:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short :: RIP.Ptr RIP.CUShort
unsigned_short =
  RIP.unsafePerformIO hs_bindgen_20f280e98d58aacc

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int@
foreign import ccall unsafe "hs_bindgen_78b4f07c60ef3ef7" hs_bindgen_78b4f07c60ef3ef7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int@
hs_bindgen_78b4f07c60ef3ef7 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_78b4f07c60ef3ef7 =
  RIP.fromFFIType hs_bindgen_78b4f07c60ef3ef7_base

{-# NOINLINE ordinary_signed_int #-}
{-| __C declaration:__ @ordinary_signed_int@

    __defined at:__ @comprehensive\/c2hsc.h 100:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int :: RIP.Ptr RIP.CInt
ordinary_signed_int =
  RIP.unsafePerformIO hs_bindgen_78b4f07c60ef3ef7

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int@
foreign import ccall unsafe "hs_bindgen_41643d3737e9213c" hs_bindgen_41643d3737e9213c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int@
hs_bindgen_41643d3737e9213c :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_41643d3737e9213c =
  RIP.fromFFIType hs_bindgen_41643d3737e9213c_base

{-# NOINLINE explicit_signed_int #-}
{-| __C declaration:__ @explicit_signed_int@

    __defined at:__ @comprehensive\/c2hsc.h 101:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int :: RIP.Ptr RIP.CInt
explicit_signed_int =
  RIP.unsafePerformIO hs_bindgen_41643d3737e9213c

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int@
foreign import ccall unsafe "hs_bindgen_6e686e175c956cb8" hs_bindgen_6e686e175c956cb8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int@
hs_bindgen_6e686e175c956cb8 :: IO (RIP.Ptr RIP.CUInt)
hs_bindgen_6e686e175c956cb8 =
  RIP.fromFFIType hs_bindgen_6e686e175c956cb8_base

{-# NOINLINE unsigned_int #-}
{-| __C declaration:__ @unsigned_int@

    __defined at:__ @comprehensive\/c2hsc.h 102:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int :: RIP.Ptr RIP.CUInt
unsigned_int =
  RIP.unsafePerformIO hs_bindgen_6e686e175c956cb8

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long@
foreign import ccall unsafe "hs_bindgen_6a41c38f7fbb41d9" hs_bindgen_6a41c38f7fbb41d9_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long@
hs_bindgen_6a41c38f7fbb41d9 :: IO (RIP.Ptr RIP.CLong)
hs_bindgen_6a41c38f7fbb41d9 =
  RIP.fromFFIType hs_bindgen_6a41c38f7fbb41d9_base

{-# NOINLINE ordinary_signed_long #-}
{-| __C declaration:__ @ordinary_signed_long@

    __defined at:__ @comprehensive\/c2hsc.h 104:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long :: RIP.Ptr RIP.CLong
ordinary_signed_long =
  RIP.unsafePerformIO hs_bindgen_6a41c38f7fbb41d9

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long@
foreign import ccall unsafe "hs_bindgen_a3b5191325da8230" hs_bindgen_a3b5191325da8230_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long@
hs_bindgen_a3b5191325da8230 :: IO (RIP.Ptr RIP.CLong)
hs_bindgen_a3b5191325da8230 =
  RIP.fromFFIType hs_bindgen_a3b5191325da8230_base

{-# NOINLINE explicit_signed_long #-}
{-| __C declaration:__ @explicit_signed_long@

    __defined at:__ @comprehensive\/c2hsc.h 105:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long :: RIP.Ptr RIP.CLong
explicit_signed_long =
  RIP.unsafePerformIO hs_bindgen_a3b5191325da8230

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long@
foreign import ccall unsafe "hs_bindgen_df563b6d35ec4e8f" hs_bindgen_df563b6d35ec4e8f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long@
hs_bindgen_df563b6d35ec4e8f :: IO (RIP.Ptr RIP.CULong)
hs_bindgen_df563b6d35ec4e8f =
  RIP.fromFFIType hs_bindgen_df563b6d35ec4e8f_base

{-# NOINLINE unsigned_long #-}
{-| __C declaration:__ @unsigned_long@

    __defined at:__ @comprehensive\/c2hsc.h 106:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long :: RIP.Ptr RIP.CULong
unsigned_long =
  RIP.unsafePerformIO hs_bindgen_df563b6d35ec4e8f

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long@
foreign import ccall unsafe "hs_bindgen_59c6159f8798505a" hs_bindgen_59c6159f8798505a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long@
hs_bindgen_59c6159f8798505a :: IO (RIP.Ptr RIP.CLLong)
hs_bindgen_59c6159f8798505a =
  RIP.fromFFIType hs_bindgen_59c6159f8798505a_base

{-# NOINLINE ordinary_signed_long_long #-}
{-| __C declaration:__ @ordinary_signed_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 108:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long :: RIP.Ptr RIP.CLLong
ordinary_signed_long_long =
  RIP.unsafePerformIO hs_bindgen_59c6159f8798505a

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long@
foreign import ccall unsafe "hs_bindgen_bfd63280c68460d2" hs_bindgen_bfd63280c68460d2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long@
hs_bindgen_bfd63280c68460d2 :: IO (RIP.Ptr RIP.CLLong)
hs_bindgen_bfd63280c68460d2 =
  RIP.fromFFIType hs_bindgen_bfd63280c68460d2_base

{-# NOINLINE explicit_signed_long_long #-}
{-| __C declaration:__ @explicit_signed_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 109:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long :: RIP.Ptr RIP.CLLong
explicit_signed_long_long =
  RIP.unsafePerformIO hs_bindgen_bfd63280c68460d2

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long@
foreign import ccall unsafe "hs_bindgen_8654be9a041c2116" hs_bindgen_8654be9a041c2116_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long@
hs_bindgen_8654be9a041c2116 :: IO (RIP.Ptr RIP.CULLong)
hs_bindgen_8654be9a041c2116 =
  RIP.fromFFIType hs_bindgen_8654be9a041c2116_base

{-# NOINLINE unsigned_long_long #-}
{-| __C declaration:__ @unsigned_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 110:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long :: RIP.Ptr RIP.CULLong
unsigned_long_long =
  RIP.unsafePerformIO hs_bindgen_8654be9a041c2116

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_void_pointer@
foreign import ccall unsafe "hs_bindgen_d6cafaa0834895a2" hs_bindgen_d6cafaa0834895a2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_void_pointer@
hs_bindgen_d6cafaa0834895a2 :: IO (RIP.Ptr (RIP.Ptr RIP.Void))
hs_bindgen_d6cafaa0834895a2 =
  RIP.fromFFIType hs_bindgen_d6cafaa0834895a2_base

{-# NOINLINE ordinary_void_pointer #-}
{-| Pointers: primitive types which cannot be signed

__C declaration:__ @ordinary_void_pointer@

__defined at:__ @comprehensive\/c2hsc.h 116:21@

__exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_void_pointer :: RIP.Ptr (RIP.Ptr RIP.Void)
ordinary_void_pointer =
  RIP.unsafePerformIO hs_bindgen_d6cafaa0834895a2

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_pointer@
foreign import ccall unsafe "hs_bindgen_35ef3ffa1a5167f7" hs_bindgen_35ef3ffa1a5167f7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_pointer@
hs_bindgen_35ef3ffa1a5167f7 :: IO (RIP.Ptr (RIP.Ptr RIP.CFloat))
hs_bindgen_35ef3ffa1a5167f7 =
  RIP.fromFFIType hs_bindgen_35ef3ffa1a5167f7_base

{-# NOINLINE ordinary_float_pointer #-}
{-| __C declaration:__ @ordinary_float_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 118:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_pointer :: RIP.Ptr (RIP.Ptr RIP.CFloat)
ordinary_float_pointer =
  RIP.unsafePerformIO hs_bindgen_35ef3ffa1a5167f7

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_pointer@
foreign import ccall unsafe "hs_bindgen_5e831159f7556399" hs_bindgen_5e831159f7556399_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_pointer@
hs_bindgen_5e831159f7556399 :: IO (RIP.Ptr (RIP.Ptr RIP.CDouble))
hs_bindgen_5e831159f7556399 =
  RIP.fromFFIType hs_bindgen_5e831159f7556399_base

{-# NOINLINE ordinary_double_pointer #-}
{-| __C declaration:__ @ordinary_double_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 119:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_pointer :: RIP.Ptr (RIP.Ptr RIP.CDouble)
ordinary_double_pointer =
  RIP.unsafePerformIO hs_bindgen_5e831159f7556399

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_char_pointer@
foreign import ccall unsafe "hs_bindgen_08236e44cabe67f4" hs_bindgen_08236e44cabe67f4_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_char_pointer@
hs_bindgen_08236e44cabe67f4 :: IO (RIP.Ptr (RIP.Ptr RIP.CChar))
hs_bindgen_08236e44cabe67f4 =
  RIP.fromFFIType hs_bindgen_08236e44cabe67f4_base

{-# NOINLINE ordinary_char_pointer #-}
{-| __C declaration:__ @ordinary_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 122:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_char_pointer :: RIP.Ptr (RIP.Ptr RIP.CChar)
ordinary_char_pointer =
  RIP.unsafePerformIO hs_bindgen_08236e44cabe67f4

-- __unique:__ @test_comprehensivec2hsc_Example_get_signed_char_pointer@
foreign import ccall unsafe "hs_bindgen_66d08eddc2fecb35" hs_bindgen_66d08eddc2fecb35_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_signed_char_pointer@
hs_bindgen_66d08eddc2fecb35 :: IO (RIP.Ptr (RIP.Ptr RIP.CSChar))
hs_bindgen_66d08eddc2fecb35 =
  RIP.fromFFIType hs_bindgen_66d08eddc2fecb35_base

{-# NOINLINE signed_char_pointer #-}
{-| __C declaration:__ @signed_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 123:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
signed_char_pointer :: RIP.Ptr (RIP.Ptr RIP.CSChar)
signed_char_pointer =
  RIP.unsafePerformIO hs_bindgen_66d08eddc2fecb35

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_pointer@
foreign import ccall unsafe "hs_bindgen_5c76c506cb4fd78e" hs_bindgen_5c76c506cb4fd78e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_pointer@
hs_bindgen_5c76c506cb4fd78e :: IO (RIP.Ptr (RIP.Ptr RIP.CUChar))
hs_bindgen_5c76c506cb4fd78e =
  RIP.fromFFIType hs_bindgen_5c76c506cb4fd78e_base

{-# NOINLINE unsigned_char_pointer #-}
{-| __C declaration:__ @unsigned_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 124:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_pointer :: RIP.Ptr (RIP.Ptr RIP.CUChar)
unsigned_char_pointer =
  RIP.unsafePerformIO hs_bindgen_5c76c506cb4fd78e

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer@
foreign import ccall unsafe "hs_bindgen_cd114d033253fe37" hs_bindgen_cd114d033253fe37_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer@
hs_bindgen_cd114d033253fe37 :: IO (RIP.Ptr (RIP.Ptr RIP.CShort))
hs_bindgen_cd114d033253fe37 =
  RIP.fromFFIType hs_bindgen_cd114d033253fe37_base

{-# NOINLINE ordinary_signed_short_pointer #-}
{-| __C declaration:__ @ordinary_signed_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 126:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_pointer :: RIP.Ptr (RIP.Ptr RIP.CShort)
ordinary_signed_short_pointer =
  RIP.unsafePerformIO hs_bindgen_cd114d033253fe37

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer@
foreign import ccall unsafe "hs_bindgen_49ffa4c56ce34e2f" hs_bindgen_49ffa4c56ce34e2f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer@
hs_bindgen_49ffa4c56ce34e2f :: IO (RIP.Ptr (RIP.Ptr RIP.CShort))
hs_bindgen_49ffa4c56ce34e2f =
  RIP.fromFFIType hs_bindgen_49ffa4c56ce34e2f_base

{-# NOINLINE explicit_signed_short_pointer #-}
{-| __C declaration:__ @explicit_signed_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 127:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_pointer :: RIP.Ptr (RIP.Ptr RIP.CShort)
explicit_signed_short_pointer =
  RIP.unsafePerformIO hs_bindgen_49ffa4c56ce34e2f

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_pointer@
foreign import ccall unsafe "hs_bindgen_fb80ad6945c7151d" hs_bindgen_fb80ad6945c7151d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_pointer@
hs_bindgen_fb80ad6945c7151d :: IO (RIP.Ptr (RIP.Ptr RIP.CUShort))
hs_bindgen_fb80ad6945c7151d =
  RIP.fromFFIType hs_bindgen_fb80ad6945c7151d_base

{-# NOINLINE unsigned_short_pointer #-}
{-| __C declaration:__ @unsigned_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 128:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_pointer :: RIP.Ptr (RIP.Ptr RIP.CUShort)
unsigned_short_pointer =
  RIP.unsafePerformIO hs_bindgen_fb80ad6945c7151d

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer@
foreign import ccall unsafe "hs_bindgen_dfef04ad64871bf8" hs_bindgen_dfef04ad64871bf8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer@
hs_bindgen_dfef04ad64871bf8 :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_dfef04ad64871bf8 =
  RIP.fromFFIType hs_bindgen_dfef04ad64871bf8_base

{-# NOINLINE ordinary_signed_int_pointer #-}
{-| __C declaration:__ @ordinary_signed_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 130:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_pointer :: RIP.Ptr (RIP.Ptr RIP.CInt)
ordinary_signed_int_pointer =
  RIP.unsafePerformIO hs_bindgen_dfef04ad64871bf8

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer@
foreign import ccall unsafe "hs_bindgen_515d5232453c4285" hs_bindgen_515d5232453c4285_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer@
hs_bindgen_515d5232453c4285 :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_515d5232453c4285 =
  RIP.fromFFIType hs_bindgen_515d5232453c4285_base

{-# NOINLINE explicit_signed_int_pointer #-}
{-| __C declaration:__ @explicit_signed_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 131:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_pointer :: RIP.Ptr (RIP.Ptr RIP.CInt)
explicit_signed_int_pointer =
  RIP.unsafePerformIO hs_bindgen_515d5232453c4285

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_pointer@
foreign import ccall unsafe "hs_bindgen_9f2724dae8d3b3fa" hs_bindgen_9f2724dae8d3b3fa_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_pointer@
hs_bindgen_9f2724dae8d3b3fa :: IO (RIP.Ptr (RIP.Ptr RIP.CUInt))
hs_bindgen_9f2724dae8d3b3fa =
  RIP.fromFFIType hs_bindgen_9f2724dae8d3b3fa_base

{-# NOINLINE unsigned_int_pointer #-}
{-| __C declaration:__ @unsigned_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 132:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_pointer :: RIP.Ptr (RIP.Ptr RIP.CUInt)
unsigned_int_pointer =
  RIP.unsafePerformIO hs_bindgen_9f2724dae8d3b3fa

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer@
foreign import ccall unsafe "hs_bindgen_a6b4a786a6644424" hs_bindgen_a6b4a786a6644424_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer@
hs_bindgen_a6b4a786a6644424 :: IO (RIP.Ptr (RIP.Ptr RIP.CLong))
hs_bindgen_a6b4a786a6644424 =
  RIP.fromFFIType hs_bindgen_a6b4a786a6644424_base

{-# NOINLINE ordinary_signed_long_pointer #-}
{-| __C declaration:__ @ordinary_signed_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 134:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CLong)
ordinary_signed_long_pointer =
  RIP.unsafePerformIO hs_bindgen_a6b4a786a6644424

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer@
foreign import ccall unsafe "hs_bindgen_4df40729ac557753" hs_bindgen_4df40729ac557753_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer@
hs_bindgen_4df40729ac557753 :: IO (RIP.Ptr (RIP.Ptr RIP.CLong))
hs_bindgen_4df40729ac557753 =
  RIP.fromFFIType hs_bindgen_4df40729ac557753_base

{-# NOINLINE explicit_signed_long_pointer #-}
{-| __C declaration:__ @explicit_signed_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 135:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CLong)
explicit_signed_long_pointer =
  RIP.unsafePerformIO hs_bindgen_4df40729ac557753

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_pointer@
foreign import ccall unsafe "hs_bindgen_30379268a6bf43b0" hs_bindgen_30379268a6bf43b0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_pointer@
hs_bindgen_30379268a6bf43b0 :: IO (RIP.Ptr (RIP.Ptr RIP.CULong))
hs_bindgen_30379268a6bf43b0 =
  RIP.fromFFIType hs_bindgen_30379268a6bf43b0_base

{-# NOINLINE unsigned_long_pointer #-}
{-| __C declaration:__ @unsigned_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 136:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CULong)
unsigned_long_pointer =
  RIP.unsafePerformIO hs_bindgen_30379268a6bf43b0

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_63dedd7015d9ed47" hs_bindgen_63dedd7015d9ed47_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer@
hs_bindgen_63dedd7015d9ed47 :: IO (RIP.Ptr (RIP.Ptr RIP.CLLong))
hs_bindgen_63dedd7015d9ed47 =
  RIP.fromFFIType hs_bindgen_63dedd7015d9ed47_base

{-# NOINLINE ordinary_signed_long_long_pointer #-}
{-| __C declaration:__ @ordinary_signed_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 138:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CLLong)
ordinary_signed_long_long_pointer =
  RIP.unsafePerformIO hs_bindgen_63dedd7015d9ed47

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_9f47ed329f79e647" hs_bindgen_9f47ed329f79e647_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer@
hs_bindgen_9f47ed329f79e647 :: IO (RIP.Ptr (RIP.Ptr RIP.CLLong))
hs_bindgen_9f47ed329f79e647 =
  RIP.fromFFIType hs_bindgen_9f47ed329f79e647_base

{-# NOINLINE explicit_signed_long_long_pointer #-}
{-| __C declaration:__ @explicit_signed_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 139:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CLLong)
explicit_signed_long_long_pointer =
  RIP.unsafePerformIO hs_bindgen_9f47ed329f79e647

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_7d2dd0a78dde4b41" hs_bindgen_7d2dd0a78dde4b41_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer@
hs_bindgen_7d2dd0a78dde4b41 :: IO (RIP.Ptr (RIP.Ptr RIP.CULLong))
hs_bindgen_7d2dd0a78dde4b41 =
  RIP.fromFFIType hs_bindgen_7d2dd0a78dde4b41_base

{-# NOINLINE unsigned_long_long_pointer #-}
{-| __C declaration:__ @unsigned_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 140:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_pointer :: RIP.Ptr (RIP.Ptr RIP.CULLong)
unsigned_long_long_pointer =
  RIP.unsafePerformIO hs_bindgen_7d2dd0a78dde4b41

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_array@
foreign import ccall unsafe "hs_bindgen_dbf314f660deda12" hs_bindgen_dbf314f660deda12_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_array@
hs_bindgen_dbf314f660deda12 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CFloat))
hs_bindgen_dbf314f660deda12 =
  RIP.fromFFIType hs_bindgen_dbf314f660deda12_base

{-# NOINLINE ordinary_float_array #-}
{-| Arrays: primitive types which cannot be signed

__C declaration:__ @ordinary_float_array@

__defined at:__ @comprehensive\/c2hsc.h 146:20@

__exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CFloat)
ordinary_float_array =
  RIP.unsafePerformIO hs_bindgen_dbf314f660deda12

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_array@
foreign import ccall unsafe "hs_bindgen_d554c83416b4ffaa" hs_bindgen_d554c83416b4ffaa_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_array@
hs_bindgen_d554c83416b4ffaa :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CDouble))
hs_bindgen_d554c83416b4ffaa =
  RIP.fromFFIType hs_bindgen_d554c83416b4ffaa_base

{-# NOINLINE ordinary_double_array #-}
{-| __C declaration:__ @ordinary_double_array@

    __defined at:__ @comprehensive\/c2hsc.h 147:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CDouble)
ordinary_double_array =
  RIP.unsafePerformIO hs_bindgen_d554c83416b4ffaa

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_char_array@
foreign import ccall unsafe "hs_bindgen_9a57c94ff3fad985" hs_bindgen_9a57c94ff3fad985_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_char_array@
hs_bindgen_9a57c94ff3fad985 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CChar))
hs_bindgen_9a57c94ff3fad985 =
  RIP.fromFFIType hs_bindgen_9a57c94ff3fad985_base

{-# NOINLINE ordinary_signed_char_array #-}
{-| __C declaration:__ @ordinary_signed_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 150:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_char_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CChar)
ordinary_signed_char_array =
  RIP.unsafePerformIO hs_bindgen_9a57c94ff3fad985

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_char_array@
foreign import ccall unsafe "hs_bindgen_335217e9e2c8fe6a" hs_bindgen_335217e9e2c8fe6a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_char_array@
hs_bindgen_335217e9e2c8fe6a :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CSChar))
hs_bindgen_335217e9e2c8fe6a =
  RIP.fromFFIType hs_bindgen_335217e9e2c8fe6a_base

{-# NOINLINE explicit_signed_char_array #-}
{-| __C declaration:__ @explicit_signed_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 151:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_char_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CSChar)
explicit_signed_char_array =
  RIP.unsafePerformIO hs_bindgen_335217e9e2c8fe6a

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_array@
foreign import ccall unsafe "hs_bindgen_1e72daddbfe2b7ac" hs_bindgen_1e72daddbfe2b7ac_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_array@
hs_bindgen_1e72daddbfe2b7ac :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CUChar))
hs_bindgen_1e72daddbfe2b7ac =
  RIP.fromFFIType hs_bindgen_1e72daddbfe2b7ac_base

{-# NOINLINE unsigned_char_array #-}
{-| __C declaration:__ @unsigned_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 152:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CUChar)
unsigned_char_array =
  RIP.unsafePerformIO hs_bindgen_1e72daddbfe2b7ac

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_array@
foreign import ccall unsafe "hs_bindgen_109462175baa2bdf" hs_bindgen_109462175baa2bdf_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_array@
hs_bindgen_109462175baa2bdf :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CShort))
hs_bindgen_109462175baa2bdf =
  RIP.fromFFIType hs_bindgen_109462175baa2bdf_base

{-# NOINLINE ordinary_signed_short_array #-}
{-| __C declaration:__ @ordinary_signed_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 154:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CShort)
ordinary_signed_short_array =
  RIP.unsafePerformIO hs_bindgen_109462175baa2bdf

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_array@
foreign import ccall unsafe "hs_bindgen_50f788c18928793d" hs_bindgen_50f788c18928793d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_array@
hs_bindgen_50f788c18928793d :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CShort))
hs_bindgen_50f788c18928793d =
  RIP.fromFFIType hs_bindgen_50f788c18928793d_base

{-# NOINLINE explicit_signed_short_array #-}
{-| __C declaration:__ @explicit_signed_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 155:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CShort)
explicit_signed_short_array =
  RIP.unsafePerformIO hs_bindgen_50f788c18928793d

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_array@
foreign import ccall unsafe "hs_bindgen_d5ccc3314ab2eb42" hs_bindgen_d5ccc3314ab2eb42_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_array@
hs_bindgen_d5ccc3314ab2eb42 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CUShort))
hs_bindgen_d5ccc3314ab2eb42 =
  RIP.fromFFIType hs_bindgen_d5ccc3314ab2eb42_base

{-# NOINLINE unsigned_short_array #-}
{-| __C declaration:__ @unsigned_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 156:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CUShort)
unsigned_short_array =
  RIP.unsafePerformIO hs_bindgen_d5ccc3314ab2eb42

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_array@
foreign import ccall unsafe "hs_bindgen_f7824c4ee10b0b90" hs_bindgen_f7824c4ee10b0b90_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_array@
hs_bindgen_f7824c4ee10b0b90 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CInt))
hs_bindgen_f7824c4ee10b0b90 =
  RIP.fromFFIType hs_bindgen_f7824c4ee10b0b90_base

{-# NOINLINE ordinary_signed_int_array #-}
{-| __C declaration:__ @ordinary_signed_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 158:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CInt)
ordinary_signed_int_array =
  RIP.unsafePerformIO hs_bindgen_f7824c4ee10b0b90

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_array@
foreign import ccall unsafe "hs_bindgen_448c7a47209c8b7a" hs_bindgen_448c7a47209c8b7a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_array@
hs_bindgen_448c7a47209c8b7a :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CInt))
hs_bindgen_448c7a47209c8b7a =
  RIP.fromFFIType hs_bindgen_448c7a47209c8b7a_base

{-# NOINLINE explicit_signed_int_array #-}
{-| __C declaration:__ @explicit_signed_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 159:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CInt)
explicit_signed_int_array =
  RIP.unsafePerformIO hs_bindgen_448c7a47209c8b7a

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_array@
foreign import ccall unsafe "hs_bindgen_66499a8ae9382b9e" hs_bindgen_66499a8ae9382b9e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_array@
hs_bindgen_66499a8ae9382b9e :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CUInt))
hs_bindgen_66499a8ae9382b9e =
  RIP.fromFFIType hs_bindgen_66499a8ae9382b9e_base

{-# NOINLINE unsigned_int_array #-}
{-| __C declaration:__ @unsigned_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 160:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CUInt)
unsigned_int_array =
  RIP.unsafePerformIO hs_bindgen_66499a8ae9382b9e

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_array@
foreign import ccall unsafe "hs_bindgen_896d3ba25d886356" hs_bindgen_896d3ba25d886356_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_array@
hs_bindgen_896d3ba25d886356 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CLong))
hs_bindgen_896d3ba25d886356 =
  RIP.fromFFIType hs_bindgen_896d3ba25d886356_base

{-# NOINLINE ordinary_signed_long_array #-}
{-| __C declaration:__ @ordinary_signed_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 162:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CLong)
ordinary_signed_long_array =
  RIP.unsafePerformIO hs_bindgen_896d3ba25d886356

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_array@
foreign import ccall unsafe "hs_bindgen_c4ec5e2d6cf5b5ed" hs_bindgen_c4ec5e2d6cf5b5ed_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_array@
hs_bindgen_c4ec5e2d6cf5b5ed :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CLong))
hs_bindgen_c4ec5e2d6cf5b5ed =
  RIP.fromFFIType hs_bindgen_c4ec5e2d6cf5b5ed_base

{-# NOINLINE explicit_signed_long_array #-}
{-| __C declaration:__ @explicit_signed_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 163:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CLong)
explicit_signed_long_array =
  RIP.unsafePerformIO hs_bindgen_c4ec5e2d6cf5b5ed

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_array@
foreign import ccall unsafe "hs_bindgen_e3fb79df55414b09" hs_bindgen_e3fb79df55414b09_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_array@
hs_bindgen_e3fb79df55414b09 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CULong))
hs_bindgen_e3fb79df55414b09 =
  RIP.fromFFIType hs_bindgen_e3fb79df55414b09_base

{-# NOINLINE unsigned_long_array #-}
{-| __C declaration:__ @unsigned_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 164:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CULong)
unsigned_long_array =
  RIP.unsafePerformIO hs_bindgen_e3fb79df55414b09

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_array@
foreign import ccall unsafe "hs_bindgen_02c15bdcdfc54945" hs_bindgen_02c15bdcdfc54945_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_array@
hs_bindgen_02c15bdcdfc54945 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CLLong))
hs_bindgen_02c15bdcdfc54945 =
  RIP.fromFFIType hs_bindgen_02c15bdcdfc54945_base

{-# NOINLINE ordinary_signed_long_long_array #-}
{-| __C declaration:__ @ordinary_signed_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 166:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CLLong)
ordinary_signed_long_long_array =
  RIP.unsafePerformIO hs_bindgen_02c15bdcdfc54945

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_array@
foreign import ccall unsafe "hs_bindgen_4c934e5832898551" hs_bindgen_4c934e5832898551_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_array@
hs_bindgen_4c934e5832898551 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CLLong))
hs_bindgen_4c934e5832898551 =
  RIP.fromFFIType hs_bindgen_4c934e5832898551_base

{-# NOINLINE explicit_signed_long_long_array #-}
{-| __C declaration:__ @explicit_signed_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 167:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CLLong)
explicit_signed_long_long_array =
  RIP.unsafePerformIO hs_bindgen_4c934e5832898551

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_array@
foreign import ccall unsafe "hs_bindgen_af47cf5cc0016405" hs_bindgen_af47cf5cc0016405_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_array@
hs_bindgen_af47cf5cc0016405 :: IO (RIP.Ptr ((CA.ConstantArray 10) RIP.CULLong))
hs_bindgen_af47cf5cc0016405 =
  RIP.fromFFIType hs_bindgen_af47cf5cc0016405_base

{-# NOINLINE unsigned_long_long_array #-}
{-| __C declaration:__ @unsigned_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 168:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_array :: RIP.Ptr ((CA.ConstantArray 10) RIP.CULLong)
unsigned_long_long_array =
  RIP.unsafePerformIO hs_bindgen_af47cf5cc0016405

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_void_pointer_array@
foreign import ccall unsafe "hs_bindgen_b0da135021f97e27" hs_bindgen_b0da135021f97e27_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_void_pointer_array@
hs_bindgen_b0da135021f97e27 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.Void)))
hs_bindgen_b0da135021f97e27 =
  RIP.fromFFIType hs_bindgen_b0da135021f97e27_base

{-# NOINLINE ordinary_void_pointer_array #-}
{-| Arrays of pointers

__C declaration:__ @ordinary_void_pointer_array@

__defined at:__ @comprehensive\/c2hsc.h 174:21@

__exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_void_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.Void))
ordinary_void_pointer_array =
  RIP.unsafePerformIO hs_bindgen_b0da135021f97e27

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_pointer_array@
foreign import ccall unsafe "hs_bindgen_41ce3a49f177f5a8" hs_bindgen_41ce3a49f177f5a8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_float_pointer_array@
hs_bindgen_41ce3a49f177f5a8 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CFloat)))
hs_bindgen_41ce3a49f177f5a8 =
  RIP.fromFFIType hs_bindgen_41ce3a49f177f5a8_base

{-# NOINLINE ordinary_float_pointer_array #-}
{-| __C declaration:__ @ordinary_float_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 176:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CFloat))
ordinary_float_pointer_array =
  RIP.unsafePerformIO hs_bindgen_41ce3a49f177f5a8

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_pointer_array@
foreign import ccall unsafe "hs_bindgen_cfe3890477df51c8" hs_bindgen_cfe3890477df51c8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_double_pointer_array@
hs_bindgen_cfe3890477df51c8 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CDouble)))
hs_bindgen_cfe3890477df51c8 =
  RIP.fromFFIType hs_bindgen_cfe3890477df51c8_base

{-# NOINLINE ordinary_double_pointer_array #-}
{-| __C declaration:__ @ordinary_double_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 177:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CDouble))
ordinary_double_pointer_array =
  RIP.unsafePerformIO hs_bindgen_cfe3890477df51c8

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_e92be5e29785f22b" hs_bindgen_e92be5e29785f22b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_char_pointer_array@
hs_bindgen_e92be5e29785f22b :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CChar)))
hs_bindgen_e92be5e29785f22b =
  RIP.fromFFIType hs_bindgen_e92be5e29785f22b_base

{-# NOINLINE ordinary_signed_char_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 180:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_char_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CChar))
ordinary_signed_char_pointer_array =
  RIP.unsafePerformIO hs_bindgen_e92be5e29785f22b

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_c8caf99249e8ad58" hs_bindgen_c8caf99249e8ad58_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_char_pointer_array@
hs_bindgen_c8caf99249e8ad58 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CSChar)))
hs_bindgen_c8caf99249e8ad58 =
  RIP.fromFFIType hs_bindgen_c8caf99249e8ad58_base

{-# NOINLINE explicit_signed_char_pointer_array #-}
{-| __C declaration:__ @explicit_signed_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 181:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_char_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CSChar))
explicit_signed_char_pointer_array =
  RIP.unsafePerformIO hs_bindgen_c8caf99249e8ad58

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_7028963176c7a21d" hs_bindgen_7028963176c7a21d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_char_pointer_array@
hs_bindgen_7028963176c7a21d :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUChar)))
hs_bindgen_7028963176c7a21d =
  RIP.fromFFIType hs_bindgen_7028963176c7a21d_base

{-# NOINLINE unsigned_char_pointer_array #-}
{-| __C declaration:__ @unsigned_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 182:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUChar))
unsigned_char_pointer_array =
  RIP.unsafePerformIO hs_bindgen_7028963176c7a21d

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_1866849231d566a0" hs_bindgen_1866849231d566a0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_short_pointer_array@
hs_bindgen_1866849231d566a0 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CShort)))
hs_bindgen_1866849231d566a0 =
  RIP.fromFFIType hs_bindgen_1866849231d566a0_base

{-# NOINLINE ordinary_signed_short_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 184:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CShort))
ordinary_signed_short_pointer_array =
  RIP.unsafePerformIO hs_bindgen_1866849231d566a0

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_68ed6d444e1b6011" hs_bindgen_68ed6d444e1b6011_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_short_pointer_array@
hs_bindgen_68ed6d444e1b6011 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CShort)))
hs_bindgen_68ed6d444e1b6011 =
  RIP.fromFFIType hs_bindgen_68ed6d444e1b6011_base

{-# NOINLINE explicit_signed_short_pointer_array #-}
{-| __C declaration:__ @explicit_signed_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 185:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CShort))
explicit_signed_short_pointer_array =
  RIP.unsafePerformIO hs_bindgen_68ed6d444e1b6011

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_7c0ca48c3269649a" hs_bindgen_7c0ca48c3269649a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_short_pointer_array@
hs_bindgen_7c0ca48c3269649a :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUShort)))
hs_bindgen_7c0ca48c3269649a =
  RIP.fromFFIType hs_bindgen_7c0ca48c3269649a_base

{-# NOINLINE unsigned_short_pointer_array #-}
{-| __C declaration:__ @unsigned_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 186:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUShort))
unsigned_short_pointer_array =
  RIP.unsafePerformIO hs_bindgen_7c0ca48c3269649a

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_58e6503e8ccbeb70" hs_bindgen_58e6503e8ccbeb70_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_int_pointer_array@
hs_bindgen_58e6503e8ccbeb70 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CInt)))
hs_bindgen_58e6503e8ccbeb70 =
  RIP.fromFFIType hs_bindgen_58e6503e8ccbeb70_base

{-# NOINLINE ordinary_signed_int_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 188:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CInt))
ordinary_signed_int_pointer_array =
  RIP.unsafePerformIO hs_bindgen_58e6503e8ccbeb70

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_21b8395de873bc0a" hs_bindgen_21b8395de873bc0a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_int_pointer_array@
hs_bindgen_21b8395de873bc0a :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CInt)))
hs_bindgen_21b8395de873bc0a =
  RIP.fromFFIType hs_bindgen_21b8395de873bc0a_base

{-# NOINLINE explicit_signed_int_pointer_array #-}
{-| __C declaration:__ @explicit_signed_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 189:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CInt))
explicit_signed_int_pointer_array =
  RIP.unsafePerformIO hs_bindgen_21b8395de873bc0a

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_667d31d5b7b5106e" hs_bindgen_667d31d5b7b5106e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_int_pointer_array@
hs_bindgen_667d31d5b7b5106e :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUInt)))
hs_bindgen_667d31d5b7b5106e =
  RIP.fromFFIType hs_bindgen_667d31d5b7b5106e_base

{-# NOINLINE unsigned_int_pointer_array #-}
{-| __C declaration:__ @unsigned_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 190:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CUInt))
unsigned_int_pointer_array =
  RIP.unsafePerformIO hs_bindgen_667d31d5b7b5106e

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_eb34bb7d213dcd3d" hs_bindgen_eb34bb7d213dcd3d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_pointer_array@
hs_bindgen_eb34bb7d213dcd3d :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLong)))
hs_bindgen_eb34bb7d213dcd3d =
  RIP.fromFFIType hs_bindgen_eb34bb7d213dcd3d_base

{-# NOINLINE ordinary_signed_long_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 192:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLong))
ordinary_signed_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_eb34bb7d213dcd3d

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_3386ff9d0e54545d" hs_bindgen_3386ff9d0e54545d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_pointer_array@
hs_bindgen_3386ff9d0e54545d :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLong)))
hs_bindgen_3386ff9d0e54545d =
  RIP.fromFFIType hs_bindgen_3386ff9d0e54545d_base

{-# NOINLINE explicit_signed_long_pointer_array #-}
{-| __C declaration:__ @explicit_signed_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 193:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLong))
explicit_signed_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_3386ff9d0e54545d

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_510be86496a28c91" hs_bindgen_510be86496a28c91_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_pointer_array@
hs_bindgen_510be86496a28c91 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CULong)))
hs_bindgen_510be86496a28c91 =
  RIP.fromFFIType hs_bindgen_510be86496a28c91_base

{-# NOINLINE unsigned_long_pointer_array #-}
{-| __C declaration:__ @unsigned_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 194:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CULong))
unsigned_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_510be86496a28c91

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_25f60aa140d341a7" hs_bindgen_25f60aa140d341a7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_ordinary_signed_long_long_pointer_array@
hs_bindgen_25f60aa140d341a7 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLLong)))
hs_bindgen_25f60aa140d341a7 =
  RIP.fromFFIType hs_bindgen_25f60aa140d341a7_base

{-# NOINLINE ordinary_signed_long_long_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 196:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLLong))
ordinary_signed_long_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_25f60aa140d341a7

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_aced8afe33f53905" hs_bindgen_aced8afe33f53905_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_explicit_signed_long_long_pointer_array@
hs_bindgen_aced8afe33f53905 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLLong)))
hs_bindgen_aced8afe33f53905 =
  RIP.fromFFIType hs_bindgen_aced8afe33f53905_base

{-# NOINLINE explicit_signed_long_long_pointer_array #-}
{-| __C declaration:__ @explicit_signed_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 197:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CLLong))
explicit_signed_long_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_aced8afe33f53905

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_9534a7c84390edc4" hs_bindgen_9534a7c84390edc4_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_comprehensivec2hsc_Example_get_unsigned_long_long_pointer_array@
hs_bindgen_9534a7c84390edc4 :: IO (RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CULLong)))
hs_bindgen_9534a7c84390edc4 =
  RIP.fromFFIType hs_bindgen_9534a7c84390edc4_base

{-# NOINLINE unsigned_long_long_pointer_array #-}
{-| __C declaration:__ @unsigned_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 198:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_pointer_array :: RIP.Ptr ((CA.ConstantArray 10) (RIP.Ptr RIP.CULLong))
unsigned_long_long_pointer_array =
  RIP.unsafePerformIO hs_bindgen_9534a7c84390edc4
