{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <comprehensive/c2hsc.h>"
  , "/* test_comprehensivec2hsc\NULExample\NULget_my_array */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_2622dcc395f94c9b (void))[]) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &my_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_float */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_21e16e2a2ba90869 (void)"
  , "{"
  , "  return &ordinary_float;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_double */"
  , "__attribute__ ((const))"
  , "double *hs_bindgen_bd6327cd7cf4c8e2 (void)"
  , "{"
  , "  return &ordinary_double;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_char */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_13a66a13510c8104 (void)"
  , "{"
  , "  return &ordinary_char;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_signed_char */"
  , "__attribute__ ((const))"
  , "signed char *hs_bindgen_7aa1a566f7571369 (void)"
  , "{"
  , "  return &signed_char;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_char */"
  , "__attribute__ ((const))"
  , "unsigned char *hs_bindgen_4ac456ee11d684c3 (void)"
  , "{"
  , "  return &unsigned_char;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_short */"
  , "__attribute__ ((const))"
  , "signed short *hs_bindgen_8054b346ac115086 (void)"
  , "{"
  , "  return &ordinary_signed_short;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_short */"
  , "__attribute__ ((const))"
  , "signed short *hs_bindgen_c910854d0790efad (void)"
  , "{"
  , "  return &explicit_signed_short;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_short */"
  , "__attribute__ ((const))"
  , "unsigned short *hs_bindgen_dfdc6931520c05f3 (void)"
  , "{"
  , "  return &unsigned_short;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_int */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_9f8a1b25b8d369e0 (void)"
  , "{"
  , "  return &ordinary_signed_int;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_int */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_7cc3c84502bdeca6 (void)"
  , "{"
  , "  return &explicit_signed_int;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_int */"
  , "__attribute__ ((const))"
  , "unsigned int *hs_bindgen_b7e08b8cbd3da48f (void)"
  , "{"
  , "  return &unsigned_int;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long */"
  , "__attribute__ ((const))"
  , "signed long *hs_bindgen_2721878f9fd90279 (void)"
  , "{"
  , "  return &ordinary_signed_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long */"
  , "__attribute__ ((const))"
  , "signed long *hs_bindgen_39fd2950efcc7959 (void)"
  , "{"
  , "  return &explicit_signed_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long */"
  , "__attribute__ ((const))"
  , "unsigned long *hs_bindgen_205667776a8e5ea8 (void)"
  , "{"
  , "  return &unsigned_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_long */"
  , "__attribute__ ((const))"
  , "signed long long *hs_bindgen_4f830ba20c4feae9 (void)"
  , "{"
  , "  return &ordinary_signed_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_long */"
  , "__attribute__ ((const))"
  , "signed long long *hs_bindgen_2f42cb3fc52cbc75 (void)"
  , "{"
  , "  return &explicit_signed_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_long */"
  , "__attribute__ ((const))"
  , "unsigned long long *hs_bindgen_13ad7287ca75d447 (void)"
  , "{"
  , "  return &unsigned_long_long;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_void_pointer */"
  , "__attribute__ ((const))"
  , "void **hs_bindgen_0bbc03233c4b0f2e (void)"
  , "{"
  , "  return &ordinary_void_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_float_pointer */"
  , "__attribute__ ((const))"
  , "float **hs_bindgen_968ab6084f487ad3 (void)"
  , "{"
  , "  return &ordinary_float_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_double_pointer */"
  , "__attribute__ ((const))"
  , "double **hs_bindgen_cf0a5c4a0fea7fb2 (void)"
  , "{"
  , "  return &ordinary_double_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_char_pointer */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_8863b918f48bfcda (void)"
  , "{"
  , "  return &ordinary_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_signed_char_pointer */"
  , "__attribute__ ((const))"
  , "signed char **hs_bindgen_0381959757bb5a48 (void)"
  , "{"
  , "  return &signed_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_char_pointer */"
  , "__attribute__ ((const))"
  , "unsigned char **hs_bindgen_df332feaa8a271aa (void)"
  , "{"
  , "  return &unsigned_char_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_short_pointer */"
  , "__attribute__ ((const))"
  , "signed short **hs_bindgen_2d800fd053199652 (void)"
  , "{"
  , "  return &ordinary_signed_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_short_pointer */"
  , "__attribute__ ((const))"
  , "signed short **hs_bindgen_aeb6eb527d2051f6 (void)"
  , "{"
  , "  return &explicit_signed_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_short_pointer */"
  , "__attribute__ ((const))"
  , "unsigned short **hs_bindgen_d05cc7e3e854b45c (void)"
  , "{"
  , "  return &unsigned_short_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_int_pointer */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_3ba44440a200845d (void)"
  , "{"
  , "  return &ordinary_signed_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_int_pointer */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_09c4c14284976415 (void)"
  , "{"
  , "  return &explicit_signed_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_int_pointer */"
  , "__attribute__ ((const))"
  , "unsigned int **hs_bindgen_1f0520eb1fba65a1 (void)"
  , "{"
  , "  return &unsigned_int_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long **hs_bindgen_a1c0a9b340239d2d (void)"
  , "{"
  , "  return &ordinary_signed_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long **hs_bindgen_aa1e13220bcaf0a5 (void)"
  , "{"
  , "  return &explicit_signed_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_pointer */"
  , "__attribute__ ((const))"
  , "unsigned long **hs_bindgen_64cc8fba76a16812 (void)"
  , "{"
  , "  return &unsigned_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long long **hs_bindgen_79e142df5a2d59bd (void)"
  , "{"
  , "  return &ordinary_signed_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_long_pointer */"
  , "__attribute__ ((const))"
  , "signed long long **hs_bindgen_9b6195ab668b156c (void)"
  , "{"
  , "  return &explicit_signed_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_long_pointer */"
  , "__attribute__ ((const))"
  , "unsigned long long **hs_bindgen_47709c5f27d6e091 (void)"
  , "{"
  , "  return &unsigned_long_long_pointer;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_float_array */"
  , "__attribute__ ((const))"
  , "float (*hs_bindgen_bffd9f5553245acc (void))[10]"
  , "{"
  , "  return &ordinary_float_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_double_array */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_3fe794ed2dce4822 (void))[10]"
  , "{"
  , "  return &ordinary_double_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_char_array */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_26fa998771d69490 (void))[10]"
  , "{"
  , "  return &ordinary_signed_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_char_array */"
  , "__attribute__ ((const))"
  , "signed char (*hs_bindgen_58e8ed84698666a6 (void))[10]"
  , "{"
  , "  return &explicit_signed_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_char_array */"
  , "__attribute__ ((const))"
  , "unsigned char (*hs_bindgen_0d1118466faed44b (void))[10]"
  , "{"
  , "  return &unsigned_char_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_short_array */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_ff11e1daa419615f (void))[10]"
  , "{"
  , "  return &ordinary_signed_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_short_array */"
  , "__attribute__ ((const))"
  , "signed short (*hs_bindgen_5dece7ec60a48530 (void))[10]"
  , "{"
  , "  return &explicit_signed_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_short_array */"
  , "__attribute__ ((const))"
  , "unsigned short (*hs_bindgen_284afbebc98525e2 (void))[10]"
  , "{"
  , "  return &unsigned_short_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_int_array */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3b8419700aed51d3 (void))[10]"
  , "{"
  , "  return &ordinary_signed_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_int_array */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bbabe8232d320abb (void))[10]"
  , "{"
  , "  return &explicit_signed_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_int_array */"
  , "__attribute__ ((const))"
  , "unsigned int (*hs_bindgen_19db64d10418b8ab (void))[10]"
  , "{"
  , "  return &unsigned_int_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_array */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_71938950e92d912c (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_array */"
  , "__attribute__ ((const))"
  , "signed long (*hs_bindgen_97ab2b478646c979 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_array */"
  , "__attribute__ ((const))"
  , "unsigned long (*hs_bindgen_0f4f182f3b4c6213 (void))[10]"
  , "{"
  , "  return &unsigned_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_long_array */"
  , "__attribute__ ((const))"
  , "signed long long (*hs_bindgen_9622c067a9a8e2fe (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_long_array */"
  , "__attribute__ ((const))"
  , "signed long long (*hs_bindgen_8d1418935516dee5 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_long_array */"
  , "__attribute__ ((const))"
  , "unsigned long long (*hs_bindgen_c3e1a50c4b9e98c1 (void))[10]"
  , "{"
  , "  return &unsigned_long_long_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_void_pointer_array */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_aa72b24dd7a19723 (void))[10]"
  , "{"
  , "  return &ordinary_void_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_float_pointer_array */"
  , "__attribute__ ((const))"
  , "float *(*hs_bindgen_8967e7241f366eaa (void))[10]"
  , "{"
  , "  return &ordinary_float_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_double_pointer_array */"
  , "__attribute__ ((const))"
  , "double *(*hs_bindgen_ad058f049e3dd852 (void))[10]"
  , "{"
  , "  return &ordinary_double_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_char_pointer_array */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_787808c4d94ecf2c (void))[10]"
  , "{"
  , "  return &ordinary_signed_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_char_pointer_array */"
  , "__attribute__ ((const))"
  , "signed char *(*hs_bindgen_698ed2edfcd69572 (void))[10]"
  , "{"
  , "  return &explicit_signed_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_char_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned char *(*hs_bindgen_5faf82b1ae2a3fcc (void))[10]"
  , "{"
  , "  return &unsigned_char_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_short_pointer_array */"
  , "__attribute__ ((const))"
  , "signed short *(*hs_bindgen_dcede94e25265b33 (void))[10]"
  , "{"
  , "  return &ordinary_signed_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_short_pointer_array */"
  , "__attribute__ ((const))"
  , "signed short *(*hs_bindgen_ab919aca1a36c79e (void))[10]"
  , "{"
  , "  return &explicit_signed_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_short_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned short *(*hs_bindgen_61035b451e40515b (void))[10]"
  , "{"
  , "  return &unsigned_short_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_int_pointer_array */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_9469d7db67915069 (void))[10]"
  , "{"
  , "  return &ordinary_signed_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_int_pointer_array */"
  , "__attribute__ ((const))"
  , "signed int *(*hs_bindgen_f91805449f350d60 (void))[10]"
  , "{"
  , "  return &explicit_signed_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_int_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned int *(*hs_bindgen_21050cdd8f9f4b77 (void))[10]"
  , "{"
  , "  return &unsigned_int_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long *(*hs_bindgen_f4b5a538b277cb41 (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long *(*hs_bindgen_117c84c771f981e6 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned long *(*hs_bindgen_3faae3431186a4c9 (void))[10]"
  , "{"
  , "  return &unsigned_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_ordinary_signed_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long long *(*hs_bindgen_ef00061e925efc12 (void))[10]"
  , "{"
  , "  return &ordinary_signed_long_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_explicit_signed_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "signed long long *(*hs_bindgen_f0c77bf64e19d230 (void))[10]"
  , "{"
  , "  return &explicit_signed_long_long_pointer_array;"
  , "}"
  , "/* test_comprehensivec2hsc\NULExample\NULget_unsigned_long_long_pointer_array */"
  , "__attribute__ ((const))"
  , "unsigned long long *(*hs_bindgen_44dfca71c8e870e7 (void))[10]"
  , "{"
  , "  return &unsigned_long_long_pointer_array;"
  , "}"
  ]))

-- __unique:__ @test_comprehensivec2hsc Example get_my_array@
foreign import ccall unsafe "hs_bindgen_2622dcc395f94c9b" hs_bindgen_2622dcc395f94c9b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_my_array@
hs_bindgen_2622dcc395f94c9b :: IO (BG.Ptr (IA.IncompleteArray (BG.FunPtr (BG.CInt -> IO BG.CInt))))
hs_bindgen_2622dcc395f94c9b =
  fmap BG.fromFFIType hs_bindgen_2622dcc395f94c9b_base

{-# NOINLINE my_array #-}
{-| __C declaration:__ @my_array@

    __defined at:__ @comprehensive\/c2hsc.h 26:14@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
my_array :: BG.Ptr (IA.IncompleteArray (BG.FunPtr (BG.CInt -> IO BG.CInt)))
my_array =
  BG.unsafePerformIO hs_bindgen_2622dcc395f94c9b

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float@
foreign import ccall unsafe "hs_bindgen_21e16e2a2ba90869" hs_bindgen_21e16e2a2ba90869_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float@
hs_bindgen_21e16e2a2ba90869 :: IO (BG.Ptr BG.CFloat)
hs_bindgen_21e16e2a2ba90869 =
  fmap BG.fromFFIType hs_bindgen_21e16e2a2ba90869_base

{-# NOINLINE ordinary_float #-}
{-| Primitive types

    NOTE: Here and elsewhere the original test suite has some references to @long double@ , some commented out, and some confusingly not, with an expectation of a translation to @CDouble@ ; the latter I think is a bug in the original test suite. Here we leave all of these test cases in; we emit an "unsupported" message for these and omit the declaration in the generated bindings.

    __C declaration:__ @ordinary_float@

    __defined at:__ @comprehensive\/c2hsc.h 88:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float :: BG.Ptr BG.CFloat
ordinary_float =
  BG.unsafePerformIO hs_bindgen_21e16e2a2ba90869

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double@
foreign import ccall unsafe "hs_bindgen_bd6327cd7cf4c8e2" hs_bindgen_bd6327cd7cf4c8e2_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double@
hs_bindgen_bd6327cd7cf4c8e2 :: IO (BG.Ptr BG.CDouble)
hs_bindgen_bd6327cd7cf4c8e2 =
  fmap BG.fromFFIType hs_bindgen_bd6327cd7cf4c8e2_base

{-# NOINLINE ordinary_double #-}
{-| __C declaration:__ @ordinary_double@

    __defined at:__ @comprehensive\/c2hsc.h 89:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double :: BG.Ptr BG.CDouble
ordinary_double =
  BG.unsafePerformIO hs_bindgen_bd6327cd7cf4c8e2

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_char@
foreign import ccall unsafe "hs_bindgen_13a66a13510c8104" hs_bindgen_13a66a13510c8104_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_char@
hs_bindgen_13a66a13510c8104 :: IO (BG.Ptr BG.CChar)
hs_bindgen_13a66a13510c8104 =
  fmap BG.fromFFIType hs_bindgen_13a66a13510c8104_base

{-# NOINLINE ordinary_char #-}
{-| __C declaration:__ @ordinary_char@

    __defined at:__ @comprehensive\/c2hsc.h 92:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_char :: BG.Ptr BG.CChar
ordinary_char =
  BG.unsafePerformIO hs_bindgen_13a66a13510c8104

-- __unique:__ @test_comprehensivec2hsc Example get_signed_char@
foreign import ccall unsafe "hs_bindgen_7aa1a566f7571369" hs_bindgen_7aa1a566f7571369_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_signed_char@
hs_bindgen_7aa1a566f7571369 :: IO (BG.Ptr BG.CSChar)
hs_bindgen_7aa1a566f7571369 =
  fmap BG.fromFFIType hs_bindgen_7aa1a566f7571369_base

{-# NOINLINE signed_char #-}
{-| __C declaration:__ @signed_char@

    __defined at:__ @comprehensive\/c2hsc.h 93:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
signed_char :: BG.Ptr BG.CSChar
signed_char =
  BG.unsafePerformIO hs_bindgen_7aa1a566f7571369

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char@
foreign import ccall unsafe "hs_bindgen_4ac456ee11d684c3" hs_bindgen_4ac456ee11d684c3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char@
hs_bindgen_4ac456ee11d684c3 :: IO (BG.Ptr BG.CUChar)
hs_bindgen_4ac456ee11d684c3 =
  fmap BG.fromFFIType hs_bindgen_4ac456ee11d684c3_base

{-# NOINLINE unsigned_char #-}
{-| __C declaration:__ @unsigned_char@

    __defined at:__ @comprehensive\/c2hsc.h 94:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char :: BG.Ptr BG.CUChar
unsigned_char =
  BG.unsafePerformIO hs_bindgen_4ac456ee11d684c3

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short@
foreign import ccall unsafe "hs_bindgen_8054b346ac115086" hs_bindgen_8054b346ac115086_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short@
hs_bindgen_8054b346ac115086 :: IO (BG.Ptr BG.CShort)
hs_bindgen_8054b346ac115086 =
  fmap BG.fromFFIType hs_bindgen_8054b346ac115086_base

{-# NOINLINE ordinary_signed_short #-}
{-| __C declaration:__ @ordinary_signed_short@

    __defined at:__ @comprehensive\/c2hsc.h 96:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short :: BG.Ptr BG.CShort
ordinary_signed_short =
  BG.unsafePerformIO hs_bindgen_8054b346ac115086

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short@
foreign import ccall unsafe "hs_bindgen_c910854d0790efad" hs_bindgen_c910854d0790efad_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short@
hs_bindgen_c910854d0790efad :: IO (BG.Ptr BG.CShort)
hs_bindgen_c910854d0790efad =
  fmap BG.fromFFIType hs_bindgen_c910854d0790efad_base

{-# NOINLINE explicit_signed_short #-}
{-| __C declaration:__ @explicit_signed_short@

    __defined at:__ @comprehensive\/c2hsc.h 97:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short :: BG.Ptr BG.CShort
explicit_signed_short =
  BG.unsafePerformIO hs_bindgen_c910854d0790efad

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short@
foreign import ccall unsafe "hs_bindgen_dfdc6931520c05f3" hs_bindgen_dfdc6931520c05f3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short@
hs_bindgen_dfdc6931520c05f3 :: IO (BG.Ptr BG.CUShort)
hs_bindgen_dfdc6931520c05f3 =
  fmap BG.fromFFIType hs_bindgen_dfdc6931520c05f3_base

{-# NOINLINE unsigned_short #-}
{-| __C declaration:__ @unsigned_short@

    __defined at:__ @comprehensive\/c2hsc.h 98:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short :: BG.Ptr BG.CUShort
unsigned_short =
  BG.unsafePerformIO hs_bindgen_dfdc6931520c05f3

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int@
foreign import ccall unsafe "hs_bindgen_9f8a1b25b8d369e0" hs_bindgen_9f8a1b25b8d369e0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int@
hs_bindgen_9f8a1b25b8d369e0 :: IO (BG.Ptr BG.CInt)
hs_bindgen_9f8a1b25b8d369e0 =
  fmap BG.fromFFIType hs_bindgen_9f8a1b25b8d369e0_base

{-# NOINLINE ordinary_signed_int #-}
{-| __C declaration:__ @ordinary_signed_int@

    __defined at:__ @comprehensive\/c2hsc.h 100:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int :: BG.Ptr BG.CInt
ordinary_signed_int =
  BG.unsafePerformIO hs_bindgen_9f8a1b25b8d369e0

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int@
foreign import ccall unsafe "hs_bindgen_7cc3c84502bdeca6" hs_bindgen_7cc3c84502bdeca6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int@
hs_bindgen_7cc3c84502bdeca6 :: IO (BG.Ptr BG.CInt)
hs_bindgen_7cc3c84502bdeca6 =
  fmap BG.fromFFIType hs_bindgen_7cc3c84502bdeca6_base

{-# NOINLINE explicit_signed_int #-}
{-| __C declaration:__ @explicit_signed_int@

    __defined at:__ @comprehensive\/c2hsc.h 101:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int :: BG.Ptr BG.CInt
explicit_signed_int =
  BG.unsafePerformIO hs_bindgen_7cc3c84502bdeca6

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int@
foreign import ccall unsafe "hs_bindgen_b7e08b8cbd3da48f" hs_bindgen_b7e08b8cbd3da48f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int@
hs_bindgen_b7e08b8cbd3da48f :: IO (BG.Ptr BG.CUInt)
hs_bindgen_b7e08b8cbd3da48f =
  fmap BG.fromFFIType hs_bindgen_b7e08b8cbd3da48f_base

{-# NOINLINE unsigned_int #-}
{-| __C declaration:__ @unsigned_int@

    __defined at:__ @comprehensive\/c2hsc.h 102:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int :: BG.Ptr BG.CUInt
unsigned_int =
  BG.unsafePerformIO hs_bindgen_b7e08b8cbd3da48f

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long@
foreign import ccall unsafe "hs_bindgen_2721878f9fd90279" hs_bindgen_2721878f9fd90279_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long@
hs_bindgen_2721878f9fd90279 :: IO (BG.Ptr BG.CLong)
hs_bindgen_2721878f9fd90279 =
  fmap BG.fromFFIType hs_bindgen_2721878f9fd90279_base

{-# NOINLINE ordinary_signed_long #-}
{-| __C declaration:__ @ordinary_signed_long@

    __defined at:__ @comprehensive\/c2hsc.h 104:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long :: BG.Ptr BG.CLong
ordinary_signed_long =
  BG.unsafePerformIO hs_bindgen_2721878f9fd90279

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long@
foreign import ccall unsafe "hs_bindgen_39fd2950efcc7959" hs_bindgen_39fd2950efcc7959_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long@
hs_bindgen_39fd2950efcc7959 :: IO (BG.Ptr BG.CLong)
hs_bindgen_39fd2950efcc7959 =
  fmap BG.fromFFIType hs_bindgen_39fd2950efcc7959_base

{-# NOINLINE explicit_signed_long #-}
{-| __C declaration:__ @explicit_signed_long@

    __defined at:__ @comprehensive\/c2hsc.h 105:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long :: BG.Ptr BG.CLong
explicit_signed_long =
  BG.unsafePerformIO hs_bindgen_39fd2950efcc7959

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long@
foreign import ccall unsafe "hs_bindgen_205667776a8e5ea8" hs_bindgen_205667776a8e5ea8_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long@
hs_bindgen_205667776a8e5ea8 :: IO (BG.Ptr BG.CULong)
hs_bindgen_205667776a8e5ea8 =
  fmap BG.fromFFIType hs_bindgen_205667776a8e5ea8_base

{-# NOINLINE unsigned_long #-}
{-| __C declaration:__ @unsigned_long@

    __defined at:__ @comprehensive\/c2hsc.h 106:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long :: BG.Ptr BG.CULong
unsigned_long =
  BG.unsafePerformIO hs_bindgen_205667776a8e5ea8

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long@
foreign import ccall unsafe "hs_bindgen_4f830ba20c4feae9" hs_bindgen_4f830ba20c4feae9_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long@
hs_bindgen_4f830ba20c4feae9 :: IO (BG.Ptr BG.CLLong)
hs_bindgen_4f830ba20c4feae9 =
  fmap BG.fromFFIType hs_bindgen_4f830ba20c4feae9_base

{-# NOINLINE ordinary_signed_long_long #-}
{-| __C declaration:__ @ordinary_signed_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 108:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long :: BG.Ptr BG.CLLong
ordinary_signed_long_long =
  BG.unsafePerformIO hs_bindgen_4f830ba20c4feae9

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long@
foreign import ccall unsafe "hs_bindgen_2f42cb3fc52cbc75" hs_bindgen_2f42cb3fc52cbc75_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long@
hs_bindgen_2f42cb3fc52cbc75 :: IO (BG.Ptr BG.CLLong)
hs_bindgen_2f42cb3fc52cbc75 =
  fmap BG.fromFFIType hs_bindgen_2f42cb3fc52cbc75_base

{-# NOINLINE explicit_signed_long_long #-}
{-| __C declaration:__ @explicit_signed_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 109:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long :: BG.Ptr BG.CLLong
explicit_signed_long_long =
  BG.unsafePerformIO hs_bindgen_2f42cb3fc52cbc75

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long@
foreign import ccall unsafe "hs_bindgen_13ad7287ca75d447" hs_bindgen_13ad7287ca75d447_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long@
hs_bindgen_13ad7287ca75d447 :: IO (BG.Ptr BG.CULLong)
hs_bindgen_13ad7287ca75d447 =
  fmap BG.fromFFIType hs_bindgen_13ad7287ca75d447_base

{-# NOINLINE unsigned_long_long #-}
{-| __C declaration:__ @unsigned_long_long@

    __defined at:__ @comprehensive\/c2hsc.h 110:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long :: BG.Ptr BG.CULLong
unsigned_long_long =
  BG.unsafePerformIO hs_bindgen_13ad7287ca75d447

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_void_pointer@
foreign import ccall unsafe "hs_bindgen_0bbc03233c4b0f2e" hs_bindgen_0bbc03233c4b0f2e_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_void_pointer@
hs_bindgen_0bbc03233c4b0f2e :: IO (BG.Ptr (BG.Ptr BG.Void))
hs_bindgen_0bbc03233c4b0f2e =
  fmap BG.fromFFIType hs_bindgen_0bbc03233c4b0f2e_base

{-# NOINLINE ordinary_void_pointer #-}
{-| Pointers: primitive types which cannot be signed

    __C declaration:__ @ordinary_void_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 116:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_void_pointer :: BG.Ptr (BG.Ptr BG.Void)
ordinary_void_pointer =
  BG.unsafePerformIO hs_bindgen_0bbc03233c4b0f2e

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_pointer@
foreign import ccall unsafe "hs_bindgen_968ab6084f487ad3" hs_bindgen_968ab6084f487ad3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_pointer@
hs_bindgen_968ab6084f487ad3 :: IO (BG.Ptr (BG.Ptr BG.CFloat))
hs_bindgen_968ab6084f487ad3 =
  fmap BG.fromFFIType hs_bindgen_968ab6084f487ad3_base

{-# NOINLINE ordinary_float_pointer #-}
{-| __C declaration:__ @ordinary_float_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 118:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_pointer :: BG.Ptr (BG.Ptr BG.CFloat)
ordinary_float_pointer =
  BG.unsafePerformIO hs_bindgen_968ab6084f487ad3

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_pointer@
foreign import ccall unsafe "hs_bindgen_cf0a5c4a0fea7fb2" hs_bindgen_cf0a5c4a0fea7fb2_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_pointer@
hs_bindgen_cf0a5c4a0fea7fb2 :: IO (BG.Ptr (BG.Ptr BG.CDouble))
hs_bindgen_cf0a5c4a0fea7fb2 =
  fmap BG.fromFFIType hs_bindgen_cf0a5c4a0fea7fb2_base

{-# NOINLINE ordinary_double_pointer #-}
{-| __C declaration:__ @ordinary_double_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 119:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_pointer :: BG.Ptr (BG.Ptr BG.CDouble)
ordinary_double_pointer =
  BG.unsafePerformIO hs_bindgen_cf0a5c4a0fea7fb2

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_char_pointer@
foreign import ccall unsafe "hs_bindgen_8863b918f48bfcda" hs_bindgen_8863b918f48bfcda_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_char_pointer@
hs_bindgen_8863b918f48bfcda :: IO (BG.Ptr (BG.Ptr BG.CChar))
hs_bindgen_8863b918f48bfcda =
  fmap BG.fromFFIType hs_bindgen_8863b918f48bfcda_base

{-# NOINLINE ordinary_char_pointer #-}
{-| __C declaration:__ @ordinary_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 122:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_char_pointer :: BG.Ptr (BG.Ptr BG.CChar)
ordinary_char_pointer =
  BG.unsafePerformIO hs_bindgen_8863b918f48bfcda

-- __unique:__ @test_comprehensivec2hsc Example get_signed_char_pointer@
foreign import ccall unsafe "hs_bindgen_0381959757bb5a48" hs_bindgen_0381959757bb5a48_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_signed_char_pointer@
hs_bindgen_0381959757bb5a48 :: IO (BG.Ptr (BG.Ptr BG.CSChar))
hs_bindgen_0381959757bb5a48 =
  fmap BG.fromFFIType hs_bindgen_0381959757bb5a48_base

{-# NOINLINE signed_char_pointer #-}
{-| __C declaration:__ @signed_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 123:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
signed_char_pointer :: BG.Ptr (BG.Ptr BG.CSChar)
signed_char_pointer =
  BG.unsafePerformIO hs_bindgen_0381959757bb5a48

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_pointer@
foreign import ccall unsafe "hs_bindgen_df332feaa8a271aa" hs_bindgen_df332feaa8a271aa_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_pointer@
hs_bindgen_df332feaa8a271aa :: IO (BG.Ptr (BG.Ptr BG.CUChar))
hs_bindgen_df332feaa8a271aa =
  fmap BG.fromFFIType hs_bindgen_df332feaa8a271aa_base

{-# NOINLINE unsigned_char_pointer #-}
{-| __C declaration:__ @unsigned_char_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 124:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_pointer :: BG.Ptr (BG.Ptr BG.CUChar)
unsigned_char_pointer =
  BG.unsafePerformIO hs_bindgen_df332feaa8a271aa

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_pointer@
foreign import ccall unsafe "hs_bindgen_2d800fd053199652" hs_bindgen_2d800fd053199652_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_pointer@
hs_bindgen_2d800fd053199652 :: IO (BG.Ptr (BG.Ptr BG.CShort))
hs_bindgen_2d800fd053199652 =
  fmap BG.fromFFIType hs_bindgen_2d800fd053199652_base

{-# NOINLINE ordinary_signed_short_pointer #-}
{-| __C declaration:__ @ordinary_signed_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 126:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_pointer :: BG.Ptr (BG.Ptr BG.CShort)
ordinary_signed_short_pointer =
  BG.unsafePerformIO hs_bindgen_2d800fd053199652

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_pointer@
foreign import ccall unsafe "hs_bindgen_aeb6eb527d2051f6" hs_bindgen_aeb6eb527d2051f6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_pointer@
hs_bindgen_aeb6eb527d2051f6 :: IO (BG.Ptr (BG.Ptr BG.CShort))
hs_bindgen_aeb6eb527d2051f6 =
  fmap BG.fromFFIType hs_bindgen_aeb6eb527d2051f6_base

{-# NOINLINE explicit_signed_short_pointer #-}
{-| __C declaration:__ @explicit_signed_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 127:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_pointer :: BG.Ptr (BG.Ptr BG.CShort)
explicit_signed_short_pointer =
  BG.unsafePerformIO hs_bindgen_aeb6eb527d2051f6

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_pointer@
foreign import ccall unsafe "hs_bindgen_d05cc7e3e854b45c" hs_bindgen_d05cc7e3e854b45c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_pointer@
hs_bindgen_d05cc7e3e854b45c :: IO (BG.Ptr (BG.Ptr BG.CUShort))
hs_bindgen_d05cc7e3e854b45c =
  fmap BG.fromFFIType hs_bindgen_d05cc7e3e854b45c_base

{-# NOINLINE unsigned_short_pointer #-}
{-| __C declaration:__ @unsigned_short_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 128:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_pointer :: BG.Ptr (BG.Ptr BG.CUShort)
unsigned_short_pointer =
  BG.unsafePerformIO hs_bindgen_d05cc7e3e854b45c

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_pointer@
foreign import ccall unsafe "hs_bindgen_3ba44440a200845d" hs_bindgen_3ba44440a200845d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_pointer@
hs_bindgen_3ba44440a200845d :: IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_3ba44440a200845d =
  fmap BG.fromFFIType hs_bindgen_3ba44440a200845d_base

{-# NOINLINE ordinary_signed_int_pointer #-}
{-| __C declaration:__ @ordinary_signed_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 130:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_pointer :: BG.Ptr (BG.Ptr BG.CInt)
ordinary_signed_int_pointer =
  BG.unsafePerformIO hs_bindgen_3ba44440a200845d

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_pointer@
foreign import ccall unsafe "hs_bindgen_09c4c14284976415" hs_bindgen_09c4c14284976415_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_pointer@
hs_bindgen_09c4c14284976415 :: IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_09c4c14284976415 =
  fmap BG.fromFFIType hs_bindgen_09c4c14284976415_base

{-# NOINLINE explicit_signed_int_pointer #-}
{-| __C declaration:__ @explicit_signed_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 131:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_pointer :: BG.Ptr (BG.Ptr BG.CInt)
explicit_signed_int_pointer =
  BG.unsafePerformIO hs_bindgen_09c4c14284976415

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_pointer@
foreign import ccall unsafe "hs_bindgen_1f0520eb1fba65a1" hs_bindgen_1f0520eb1fba65a1_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_pointer@
hs_bindgen_1f0520eb1fba65a1 :: IO (BG.Ptr (BG.Ptr BG.CUInt))
hs_bindgen_1f0520eb1fba65a1 =
  fmap BG.fromFFIType hs_bindgen_1f0520eb1fba65a1_base

{-# NOINLINE unsigned_int_pointer #-}
{-| __C declaration:__ @unsigned_int_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 132:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_pointer :: BG.Ptr (BG.Ptr BG.CUInt)
unsigned_int_pointer =
  BG.unsafePerformIO hs_bindgen_1f0520eb1fba65a1

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_pointer@
foreign import ccall unsafe "hs_bindgen_a1c0a9b340239d2d" hs_bindgen_a1c0a9b340239d2d_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_pointer@
hs_bindgen_a1c0a9b340239d2d :: IO (BG.Ptr (BG.Ptr BG.CLong))
hs_bindgen_a1c0a9b340239d2d =
  fmap BG.fromFFIType hs_bindgen_a1c0a9b340239d2d_base

{-# NOINLINE ordinary_signed_long_pointer #-}
{-| __C declaration:__ @ordinary_signed_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 134:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_pointer :: BG.Ptr (BG.Ptr BG.CLong)
ordinary_signed_long_pointer =
  BG.unsafePerformIO hs_bindgen_a1c0a9b340239d2d

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_pointer@
foreign import ccall unsafe "hs_bindgen_aa1e13220bcaf0a5" hs_bindgen_aa1e13220bcaf0a5_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_pointer@
hs_bindgen_aa1e13220bcaf0a5 :: IO (BG.Ptr (BG.Ptr BG.CLong))
hs_bindgen_aa1e13220bcaf0a5 =
  fmap BG.fromFFIType hs_bindgen_aa1e13220bcaf0a5_base

{-# NOINLINE explicit_signed_long_pointer #-}
{-| __C declaration:__ @explicit_signed_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 135:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_pointer :: BG.Ptr (BG.Ptr BG.CLong)
explicit_signed_long_pointer =
  BG.unsafePerformIO hs_bindgen_aa1e13220bcaf0a5

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_pointer@
foreign import ccall unsafe "hs_bindgen_64cc8fba76a16812" hs_bindgen_64cc8fba76a16812_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_pointer@
hs_bindgen_64cc8fba76a16812 :: IO (BG.Ptr (BG.Ptr BG.CULong))
hs_bindgen_64cc8fba76a16812 =
  fmap BG.fromFFIType hs_bindgen_64cc8fba76a16812_base

{-# NOINLINE unsigned_long_pointer #-}
{-| __C declaration:__ @unsigned_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 136:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_pointer :: BG.Ptr (BG.Ptr BG.CULong)
unsigned_long_pointer =
  BG.unsafePerformIO hs_bindgen_64cc8fba76a16812

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_79e142df5a2d59bd" hs_bindgen_79e142df5a2d59bd_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_pointer@
hs_bindgen_79e142df5a2d59bd :: IO (BG.Ptr (BG.Ptr BG.CLLong))
hs_bindgen_79e142df5a2d59bd =
  fmap BG.fromFFIType hs_bindgen_79e142df5a2d59bd_base

{-# NOINLINE ordinary_signed_long_long_pointer #-}
{-| __C declaration:__ @ordinary_signed_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 138:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_pointer :: BG.Ptr (BG.Ptr BG.CLLong)
ordinary_signed_long_long_pointer =
  BG.unsafePerformIO hs_bindgen_79e142df5a2d59bd

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_9b6195ab668b156c" hs_bindgen_9b6195ab668b156c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_pointer@
hs_bindgen_9b6195ab668b156c :: IO (BG.Ptr (BG.Ptr BG.CLLong))
hs_bindgen_9b6195ab668b156c =
  fmap BG.fromFFIType hs_bindgen_9b6195ab668b156c_base

{-# NOINLINE explicit_signed_long_long_pointer #-}
{-| __C declaration:__ @explicit_signed_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 139:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_pointer :: BG.Ptr (BG.Ptr BG.CLLong)
explicit_signed_long_long_pointer =
  BG.unsafePerformIO hs_bindgen_9b6195ab668b156c

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_pointer@
foreign import ccall unsafe "hs_bindgen_47709c5f27d6e091" hs_bindgen_47709c5f27d6e091_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_pointer@
hs_bindgen_47709c5f27d6e091 :: IO (BG.Ptr (BG.Ptr BG.CULLong))
hs_bindgen_47709c5f27d6e091 =
  fmap BG.fromFFIType hs_bindgen_47709c5f27d6e091_base

{-# NOINLINE unsigned_long_long_pointer #-}
{-| __C declaration:__ @unsigned_long_long_pointer@

    __defined at:__ @comprehensive\/c2hsc.h 140:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_pointer :: BG.Ptr (BG.Ptr BG.CULLong)
unsigned_long_long_pointer =
  BG.unsafePerformIO hs_bindgen_47709c5f27d6e091

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_array@
foreign import ccall unsafe "hs_bindgen_bffd9f5553245acc" hs_bindgen_bffd9f5553245acc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_array@
hs_bindgen_bffd9f5553245acc :: IO (BG.Ptr (CA.ConstantArray 10 BG.CFloat))
hs_bindgen_bffd9f5553245acc =
  fmap BG.fromFFIType hs_bindgen_bffd9f5553245acc_base

{-# NOINLINE ordinary_float_array #-}
{-| Arrays: primitive types which cannot be signed

    __C declaration:__ @ordinary_float_array@

    __defined at:__ @comprehensive\/c2hsc.h 146:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_array :: BG.Ptr (CA.ConstantArray 10 BG.CFloat)
ordinary_float_array =
  BG.unsafePerformIO hs_bindgen_bffd9f5553245acc

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_array@
foreign import ccall unsafe "hs_bindgen_3fe794ed2dce4822" hs_bindgen_3fe794ed2dce4822_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_array@
hs_bindgen_3fe794ed2dce4822 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CDouble))
hs_bindgen_3fe794ed2dce4822 =
  fmap BG.fromFFIType hs_bindgen_3fe794ed2dce4822_base

{-# NOINLINE ordinary_double_array #-}
{-| __C declaration:__ @ordinary_double_array@

    __defined at:__ @comprehensive\/c2hsc.h 147:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_array :: BG.Ptr (CA.ConstantArray 10 BG.CDouble)
ordinary_double_array =
  BG.unsafePerformIO hs_bindgen_3fe794ed2dce4822

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_char_array@
foreign import ccall unsafe "hs_bindgen_26fa998771d69490" hs_bindgen_26fa998771d69490_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_char_array@
hs_bindgen_26fa998771d69490 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CChar))
hs_bindgen_26fa998771d69490 =
  fmap BG.fromFFIType hs_bindgen_26fa998771d69490_base

{-# NOINLINE ordinary_signed_char_array #-}
{-| __C declaration:__ @ordinary_signed_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 150:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_char_array :: BG.Ptr (CA.ConstantArray 10 BG.CChar)
ordinary_signed_char_array =
  BG.unsafePerformIO hs_bindgen_26fa998771d69490

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_char_array@
foreign import ccall unsafe "hs_bindgen_58e8ed84698666a6" hs_bindgen_58e8ed84698666a6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_char_array@
hs_bindgen_58e8ed84698666a6 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CSChar))
hs_bindgen_58e8ed84698666a6 =
  fmap BG.fromFFIType hs_bindgen_58e8ed84698666a6_base

{-# NOINLINE explicit_signed_char_array #-}
{-| __C declaration:__ @explicit_signed_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 151:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_char_array :: BG.Ptr (CA.ConstantArray 10 BG.CSChar)
explicit_signed_char_array =
  BG.unsafePerformIO hs_bindgen_58e8ed84698666a6

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_array@
foreign import ccall unsafe "hs_bindgen_0d1118466faed44b" hs_bindgen_0d1118466faed44b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_array@
hs_bindgen_0d1118466faed44b :: IO (BG.Ptr (CA.ConstantArray 10 BG.CUChar))
hs_bindgen_0d1118466faed44b =
  fmap BG.fromFFIType hs_bindgen_0d1118466faed44b_base

{-# NOINLINE unsigned_char_array #-}
{-| __C declaration:__ @unsigned_char_array@

    __defined at:__ @comprehensive\/c2hsc.h 152:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_array :: BG.Ptr (CA.ConstantArray 10 BG.CUChar)
unsigned_char_array =
  BG.unsafePerformIO hs_bindgen_0d1118466faed44b

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_array@
foreign import ccall unsafe "hs_bindgen_ff11e1daa419615f" hs_bindgen_ff11e1daa419615f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_array@
hs_bindgen_ff11e1daa419615f :: IO (BG.Ptr (CA.ConstantArray 10 BG.CShort))
hs_bindgen_ff11e1daa419615f =
  fmap BG.fromFFIType hs_bindgen_ff11e1daa419615f_base

{-# NOINLINE ordinary_signed_short_array #-}
{-| __C declaration:__ @ordinary_signed_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 154:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_array :: BG.Ptr (CA.ConstantArray 10 BG.CShort)
ordinary_signed_short_array =
  BG.unsafePerformIO hs_bindgen_ff11e1daa419615f

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_array@
foreign import ccall unsafe "hs_bindgen_5dece7ec60a48530" hs_bindgen_5dece7ec60a48530_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_array@
hs_bindgen_5dece7ec60a48530 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CShort))
hs_bindgen_5dece7ec60a48530 =
  fmap BG.fromFFIType hs_bindgen_5dece7ec60a48530_base

{-# NOINLINE explicit_signed_short_array #-}
{-| __C declaration:__ @explicit_signed_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 155:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_array :: BG.Ptr (CA.ConstantArray 10 BG.CShort)
explicit_signed_short_array =
  BG.unsafePerformIO hs_bindgen_5dece7ec60a48530

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_array@
foreign import ccall unsafe "hs_bindgen_284afbebc98525e2" hs_bindgen_284afbebc98525e2_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_array@
hs_bindgen_284afbebc98525e2 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CUShort))
hs_bindgen_284afbebc98525e2 =
  fmap BG.fromFFIType hs_bindgen_284afbebc98525e2_base

{-# NOINLINE unsigned_short_array #-}
{-| __C declaration:__ @unsigned_short_array@

    __defined at:__ @comprehensive\/c2hsc.h 156:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_array :: BG.Ptr (CA.ConstantArray 10 BG.CUShort)
unsigned_short_array =
  BG.unsafePerformIO hs_bindgen_284afbebc98525e2

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_array@
foreign import ccall unsafe "hs_bindgen_3b8419700aed51d3" hs_bindgen_3b8419700aed51d3_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_array@
hs_bindgen_3b8419700aed51d3 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CInt))
hs_bindgen_3b8419700aed51d3 =
  fmap BG.fromFFIType hs_bindgen_3b8419700aed51d3_base

{-# NOINLINE ordinary_signed_int_array #-}
{-| __C declaration:__ @ordinary_signed_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 158:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_array :: BG.Ptr (CA.ConstantArray 10 BG.CInt)
ordinary_signed_int_array =
  BG.unsafePerformIO hs_bindgen_3b8419700aed51d3

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_array@
foreign import ccall unsafe "hs_bindgen_bbabe8232d320abb" hs_bindgen_bbabe8232d320abb_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_array@
hs_bindgen_bbabe8232d320abb :: IO (BG.Ptr (CA.ConstantArray 10 BG.CInt))
hs_bindgen_bbabe8232d320abb =
  fmap BG.fromFFIType hs_bindgen_bbabe8232d320abb_base

{-# NOINLINE explicit_signed_int_array #-}
{-| __C declaration:__ @explicit_signed_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 159:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_array :: BG.Ptr (CA.ConstantArray 10 BG.CInt)
explicit_signed_int_array =
  BG.unsafePerformIO hs_bindgen_bbabe8232d320abb

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_array@
foreign import ccall unsafe "hs_bindgen_19db64d10418b8ab" hs_bindgen_19db64d10418b8ab_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_array@
hs_bindgen_19db64d10418b8ab :: IO (BG.Ptr (CA.ConstantArray 10 BG.CUInt))
hs_bindgen_19db64d10418b8ab =
  fmap BG.fromFFIType hs_bindgen_19db64d10418b8ab_base

{-# NOINLINE unsigned_int_array #-}
{-| __C declaration:__ @unsigned_int_array@

    __defined at:__ @comprehensive\/c2hsc.h 160:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_array :: BG.Ptr (CA.ConstantArray 10 BG.CUInt)
unsigned_int_array =
  BG.unsafePerformIO hs_bindgen_19db64d10418b8ab

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_array@
foreign import ccall unsafe "hs_bindgen_71938950e92d912c" hs_bindgen_71938950e92d912c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_array@
hs_bindgen_71938950e92d912c :: IO (BG.Ptr (CA.ConstantArray 10 BG.CLong))
hs_bindgen_71938950e92d912c =
  fmap BG.fromFFIType hs_bindgen_71938950e92d912c_base

{-# NOINLINE ordinary_signed_long_array #-}
{-| __C declaration:__ @ordinary_signed_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 162:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CLong)
ordinary_signed_long_array =
  BG.unsafePerformIO hs_bindgen_71938950e92d912c

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_array@
foreign import ccall unsafe "hs_bindgen_97ab2b478646c979" hs_bindgen_97ab2b478646c979_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_array@
hs_bindgen_97ab2b478646c979 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CLong))
hs_bindgen_97ab2b478646c979 =
  fmap BG.fromFFIType hs_bindgen_97ab2b478646c979_base

{-# NOINLINE explicit_signed_long_array #-}
{-| __C declaration:__ @explicit_signed_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 163:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CLong)
explicit_signed_long_array =
  BG.unsafePerformIO hs_bindgen_97ab2b478646c979

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_array@
foreign import ccall unsafe "hs_bindgen_0f4f182f3b4c6213" hs_bindgen_0f4f182f3b4c6213_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_array@
hs_bindgen_0f4f182f3b4c6213 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CULong))
hs_bindgen_0f4f182f3b4c6213 =
  fmap BG.fromFFIType hs_bindgen_0f4f182f3b4c6213_base

{-# NOINLINE unsigned_long_array #-}
{-| __C declaration:__ @unsigned_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 164:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CULong)
unsigned_long_array =
  BG.unsafePerformIO hs_bindgen_0f4f182f3b4c6213

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_array@
foreign import ccall unsafe "hs_bindgen_9622c067a9a8e2fe" hs_bindgen_9622c067a9a8e2fe_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_array@
hs_bindgen_9622c067a9a8e2fe :: IO (BG.Ptr (CA.ConstantArray 10 BG.CLLong))
hs_bindgen_9622c067a9a8e2fe =
  fmap BG.fromFFIType hs_bindgen_9622c067a9a8e2fe_base

{-# NOINLINE ordinary_signed_long_long_array #-}
{-| __C declaration:__ @ordinary_signed_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 166:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CLLong)
ordinary_signed_long_long_array =
  BG.unsafePerformIO hs_bindgen_9622c067a9a8e2fe

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_array@
foreign import ccall unsafe "hs_bindgen_8d1418935516dee5" hs_bindgen_8d1418935516dee5_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_array@
hs_bindgen_8d1418935516dee5 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CLLong))
hs_bindgen_8d1418935516dee5 =
  fmap BG.fromFFIType hs_bindgen_8d1418935516dee5_base

{-# NOINLINE explicit_signed_long_long_array #-}
{-| __C declaration:__ @explicit_signed_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 167:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CLLong)
explicit_signed_long_long_array =
  BG.unsafePerformIO hs_bindgen_8d1418935516dee5

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_array@
foreign import ccall unsafe "hs_bindgen_c3e1a50c4b9e98c1" hs_bindgen_c3e1a50c4b9e98c1_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_array@
hs_bindgen_c3e1a50c4b9e98c1 :: IO (BG.Ptr (CA.ConstantArray 10 BG.CULLong))
hs_bindgen_c3e1a50c4b9e98c1 =
  fmap BG.fromFFIType hs_bindgen_c3e1a50c4b9e98c1_base

{-# NOINLINE unsigned_long_long_array #-}
{-| __C declaration:__ @unsigned_long_long_array@

    __defined at:__ @comprehensive\/c2hsc.h 168:20@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_array :: BG.Ptr (CA.ConstantArray 10 BG.CULLong)
unsigned_long_long_array =
  BG.unsafePerformIO hs_bindgen_c3e1a50c4b9e98c1

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_void_pointer_array@
foreign import ccall unsafe "hs_bindgen_aa72b24dd7a19723" hs_bindgen_aa72b24dd7a19723_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_void_pointer_array@
hs_bindgen_aa72b24dd7a19723 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.Void)))
hs_bindgen_aa72b24dd7a19723 =
  fmap BG.fromFFIType hs_bindgen_aa72b24dd7a19723_base

{-# NOINLINE ordinary_void_pointer_array #-}
{-| Arrays of pointers

    __C declaration:__ @ordinary_void_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 174:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_void_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.Void))
ordinary_void_pointer_array =
  BG.unsafePerformIO hs_bindgen_aa72b24dd7a19723

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_pointer_array@
foreign import ccall unsafe "hs_bindgen_8967e7241f366eaa" hs_bindgen_8967e7241f366eaa_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_float_pointer_array@
hs_bindgen_8967e7241f366eaa :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CFloat)))
hs_bindgen_8967e7241f366eaa =
  fmap BG.fromFFIType hs_bindgen_8967e7241f366eaa_base

{-# NOINLINE ordinary_float_pointer_array #-}
{-| __C declaration:__ @ordinary_float_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 176:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_float_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CFloat))
ordinary_float_pointer_array =
  BG.unsafePerformIO hs_bindgen_8967e7241f366eaa

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_pointer_array@
foreign import ccall unsafe "hs_bindgen_ad058f049e3dd852" hs_bindgen_ad058f049e3dd852_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_double_pointer_array@
hs_bindgen_ad058f049e3dd852 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CDouble)))
hs_bindgen_ad058f049e3dd852 =
  fmap BG.fromFFIType hs_bindgen_ad058f049e3dd852_base

{-# NOINLINE ordinary_double_pointer_array #-}
{-| __C declaration:__ @ordinary_double_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 177:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_double_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CDouble))
ordinary_double_pointer_array =
  BG.unsafePerformIO hs_bindgen_ad058f049e3dd852

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_787808c4d94ecf2c" hs_bindgen_787808c4d94ecf2c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_char_pointer_array@
hs_bindgen_787808c4d94ecf2c :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CChar)))
hs_bindgen_787808c4d94ecf2c =
  fmap BG.fromFFIType hs_bindgen_787808c4d94ecf2c_base

{-# NOINLINE ordinary_signed_char_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 180:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_char_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CChar))
ordinary_signed_char_pointer_array =
  BG.unsafePerformIO hs_bindgen_787808c4d94ecf2c

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_698ed2edfcd69572" hs_bindgen_698ed2edfcd69572_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_char_pointer_array@
hs_bindgen_698ed2edfcd69572 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CSChar)))
hs_bindgen_698ed2edfcd69572 =
  fmap BG.fromFFIType hs_bindgen_698ed2edfcd69572_base

{-# NOINLINE explicit_signed_char_pointer_array #-}
{-| __C declaration:__ @explicit_signed_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 181:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_char_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CSChar))
explicit_signed_char_pointer_array =
  BG.unsafePerformIO hs_bindgen_698ed2edfcd69572

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_pointer_array@
foreign import ccall unsafe "hs_bindgen_5faf82b1ae2a3fcc" hs_bindgen_5faf82b1ae2a3fcc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_char_pointer_array@
hs_bindgen_5faf82b1ae2a3fcc :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUChar)))
hs_bindgen_5faf82b1ae2a3fcc =
  fmap BG.fromFFIType hs_bindgen_5faf82b1ae2a3fcc_base

{-# NOINLINE unsigned_char_pointer_array #-}
{-| __C declaration:__ @unsigned_char_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 182:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_char_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUChar))
unsigned_char_pointer_array =
  BG.unsafePerformIO hs_bindgen_5faf82b1ae2a3fcc

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_dcede94e25265b33" hs_bindgen_dcede94e25265b33_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_short_pointer_array@
hs_bindgen_dcede94e25265b33 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CShort)))
hs_bindgen_dcede94e25265b33 =
  fmap BG.fromFFIType hs_bindgen_dcede94e25265b33_base

{-# NOINLINE ordinary_signed_short_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 184:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_short_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CShort))
ordinary_signed_short_pointer_array =
  BG.unsafePerformIO hs_bindgen_dcede94e25265b33

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_ab919aca1a36c79e" hs_bindgen_ab919aca1a36c79e_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_short_pointer_array@
hs_bindgen_ab919aca1a36c79e :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CShort)))
hs_bindgen_ab919aca1a36c79e =
  fmap BG.fromFFIType hs_bindgen_ab919aca1a36c79e_base

{-# NOINLINE explicit_signed_short_pointer_array #-}
{-| __C declaration:__ @explicit_signed_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 185:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_short_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CShort))
explicit_signed_short_pointer_array =
  BG.unsafePerformIO hs_bindgen_ab919aca1a36c79e

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_pointer_array@
foreign import ccall unsafe "hs_bindgen_61035b451e40515b" hs_bindgen_61035b451e40515b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_short_pointer_array@
hs_bindgen_61035b451e40515b :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUShort)))
hs_bindgen_61035b451e40515b =
  fmap BG.fromFFIType hs_bindgen_61035b451e40515b_base

{-# NOINLINE unsigned_short_pointer_array #-}
{-| __C declaration:__ @unsigned_short_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 186:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_short_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUShort))
unsigned_short_pointer_array =
  BG.unsafePerformIO hs_bindgen_61035b451e40515b

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_9469d7db67915069" hs_bindgen_9469d7db67915069_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_int_pointer_array@
hs_bindgen_9469d7db67915069 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CInt)))
hs_bindgen_9469d7db67915069 =
  fmap BG.fromFFIType hs_bindgen_9469d7db67915069_base

{-# NOINLINE ordinary_signed_int_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 188:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_int_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CInt))
ordinary_signed_int_pointer_array =
  BG.unsafePerformIO hs_bindgen_9469d7db67915069

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_f91805449f350d60" hs_bindgen_f91805449f350d60_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_int_pointer_array@
hs_bindgen_f91805449f350d60 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CInt)))
hs_bindgen_f91805449f350d60 =
  fmap BG.fromFFIType hs_bindgen_f91805449f350d60_base

{-# NOINLINE explicit_signed_int_pointer_array #-}
{-| __C declaration:__ @explicit_signed_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 189:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_int_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CInt))
explicit_signed_int_pointer_array =
  BG.unsafePerformIO hs_bindgen_f91805449f350d60

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_pointer_array@
foreign import ccall unsafe "hs_bindgen_21050cdd8f9f4b77" hs_bindgen_21050cdd8f9f4b77_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_int_pointer_array@
hs_bindgen_21050cdd8f9f4b77 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUInt)))
hs_bindgen_21050cdd8f9f4b77 =
  fmap BG.fromFFIType hs_bindgen_21050cdd8f9f4b77_base

{-# NOINLINE unsigned_int_pointer_array #-}
{-| __C declaration:__ @unsigned_int_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 190:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_int_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CUInt))
unsigned_int_pointer_array =
  BG.unsafePerformIO hs_bindgen_21050cdd8f9f4b77

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_f4b5a538b277cb41" hs_bindgen_f4b5a538b277cb41_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_pointer_array@
hs_bindgen_f4b5a538b277cb41 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLong)))
hs_bindgen_f4b5a538b277cb41 =
  fmap BG.fromFFIType hs_bindgen_f4b5a538b277cb41_base

{-# NOINLINE ordinary_signed_long_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 192:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLong))
ordinary_signed_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_f4b5a538b277cb41

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_117c84c771f981e6" hs_bindgen_117c84c771f981e6_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_pointer_array@
hs_bindgen_117c84c771f981e6 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLong)))
hs_bindgen_117c84c771f981e6 =
  fmap BG.fromFFIType hs_bindgen_117c84c771f981e6_base

{-# NOINLINE explicit_signed_long_pointer_array #-}
{-| __C declaration:__ @explicit_signed_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 193:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLong))
explicit_signed_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_117c84c771f981e6

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_3faae3431186a4c9" hs_bindgen_3faae3431186a4c9_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_pointer_array@
hs_bindgen_3faae3431186a4c9 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CULong)))
hs_bindgen_3faae3431186a4c9 =
  fmap BG.fromFFIType hs_bindgen_3faae3431186a4c9_base

{-# NOINLINE unsigned_long_pointer_array #-}
{-| __C declaration:__ @unsigned_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 194:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CULong))
unsigned_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_3faae3431186a4c9

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_ef00061e925efc12" hs_bindgen_ef00061e925efc12_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_ordinary_signed_long_long_pointer_array@
hs_bindgen_ef00061e925efc12 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLLong)))
hs_bindgen_ef00061e925efc12 =
  fmap BG.fromFFIType hs_bindgen_ef00061e925efc12_base

{-# NOINLINE ordinary_signed_long_long_pointer_array #-}
{-| __C declaration:__ @ordinary_signed_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 196:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
ordinary_signed_long_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLLong))
ordinary_signed_long_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_ef00061e925efc12

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_f0c77bf64e19d230" hs_bindgen_f0c77bf64e19d230_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_explicit_signed_long_long_pointer_array@
hs_bindgen_f0c77bf64e19d230 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLLong)))
hs_bindgen_f0c77bf64e19d230 =
  fmap BG.fromFFIType hs_bindgen_f0c77bf64e19d230_base

{-# NOINLINE explicit_signed_long_long_pointer_array #-}
{-| __C declaration:__ @explicit_signed_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 197:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
explicit_signed_long_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CLLong))
explicit_signed_long_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_f0c77bf64e19d230

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_pointer_array@
foreign import ccall unsafe "hs_bindgen_44dfca71c8e870e7" hs_bindgen_44dfca71c8e870e7_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_comprehensivec2hsc Example get_unsigned_long_long_pointer_array@
hs_bindgen_44dfca71c8e870e7 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CULLong)))
hs_bindgen_44dfca71c8e870e7 =
  fmap BG.fromFFIType hs_bindgen_44dfca71c8e870e7_base

{-# NOINLINE unsigned_long_long_pointer_array #-}
{-| __C declaration:__ @unsigned_long_long_pointer_array@

    __defined at:__ @comprehensive\/c2hsc.h 198:21@

    __exported by:__ @comprehensive\/c2hsc.h@
-}
unsigned_long_long_pointer_array :: BG.Ptr (CA.ConstantArray 10 (BG.Ptr BG.CULLong))
unsigned_long_long_pointer_array =
  BG.unsafePerformIO hs_bindgen_44dfca71c8e870e7
