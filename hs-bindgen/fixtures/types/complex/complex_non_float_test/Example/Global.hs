{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/complex_non_float_test.h>"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short */"
  , "__attribute__ ((const))"
  , "unsigned short _Complex *hs_bindgen_f3018b1e87470bdd (void)"
  , "{"
  , "  return &global_complex_unsigned_short;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_short */"
  , "__attribute__ ((const))"
  , "signed short _Complex *hs_bindgen_a553be84170a3efa (void)"
  , "{"
  , "  return &global_complex_short;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int */"
  , "__attribute__ ((const))"
  , "unsigned int _Complex *hs_bindgen_072fc08ef6d9395a (void)"
  , "{"
  , "  return &global_complex_unsigned_int;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_int */"
  , "__attribute__ ((const))"
  , "signed int _Complex *hs_bindgen_1199a9d54fe334cb (void)"
  , "{"
  , "  return &global_complex_int;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_char */"
  , "__attribute__ ((const))"
  , "char _Complex *hs_bindgen_7c8cb41a7febc6fc (void)"
  , "{"
  , "  return &global_complex_char;"
  , "}"
  ]))

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short@
foreign import ccall unsafe "hs_bindgen_f3018b1e87470bdd" hs_bindgen_f3018b1e87470bdd_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short@
hs_bindgen_f3018b1e87470bdd :: IO (RIP.Ptr (RIP.Complex RIP.CUShort))
hs_bindgen_f3018b1e87470bdd =
  RIP.fromFFIType hs_bindgen_f3018b1e87470bdd_base

{-# NOINLINE global_complex_unsigned_short #-}
{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h 3:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_short :: RIP.Ptr (RIP.Complex RIP.CUShort)
global_complex_unsigned_short =
  RIP.unsafePerformIO hs_bindgen_f3018b1e87470bdd

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_short@
foreign import ccall unsafe "hs_bindgen_a553be84170a3efa" hs_bindgen_a553be84170a3efa_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_short@
hs_bindgen_a553be84170a3efa :: IO (RIP.Ptr (RIP.Complex RIP.CShort))
hs_bindgen_a553be84170a3efa =
  RIP.fromFFIType hs_bindgen_a553be84170a3efa_base

{-# NOINLINE global_complex_short #-}
{-| __C declaration:__ @global_complex_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h 4:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_short :: RIP.Ptr (RIP.Complex RIP.CShort)
global_complex_short =
  RIP.unsafePerformIO hs_bindgen_a553be84170a3efa

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int@
foreign import ccall unsafe "hs_bindgen_072fc08ef6d9395a" hs_bindgen_072fc08ef6d9395a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int@
hs_bindgen_072fc08ef6d9395a :: IO (RIP.Ptr (RIP.Complex RIP.CUInt))
hs_bindgen_072fc08ef6d9395a =
  RIP.fromFFIType hs_bindgen_072fc08ef6d9395a_base

{-# NOINLINE global_complex_unsigned_int #-}
{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h 5:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_int :: RIP.Ptr (RIP.Complex RIP.CUInt)
global_complex_unsigned_int =
  RIP.unsafePerformIO hs_bindgen_072fc08ef6d9395a

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_int@
foreign import ccall unsafe "hs_bindgen_1199a9d54fe334cb" hs_bindgen_1199a9d54fe334cb_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_int@
hs_bindgen_1199a9d54fe334cb :: IO (RIP.Ptr (RIP.Complex RIP.CInt))
hs_bindgen_1199a9d54fe334cb =
  RIP.fromFFIType hs_bindgen_1199a9d54fe334cb_base

{-# NOINLINE global_complex_int #-}
{-| __C declaration:__ @global_complex_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h 6:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_int :: RIP.Ptr (RIP.Complex RIP.CInt)
global_complex_int =
  RIP.unsafePerformIO hs_bindgen_1199a9d54fe334cb

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_char@
foreign import ccall unsafe "hs_bindgen_7c8cb41a7febc6fc" hs_bindgen_7c8cb41a7febc6fc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_char@
hs_bindgen_7c8cb41a7febc6fc :: IO (RIP.Ptr (RIP.Complex RIP.CChar))
hs_bindgen_7c8cb41a7febc6fc =
  RIP.fromFFIType hs_bindgen_7c8cb41a7febc6fc_base

{-# NOINLINE global_complex_char #-}
{-| __C declaration:__ @global_complex_char@

    __defined at:__ @types\/complex\/complex_non_float_test.h 7:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_char :: RIP.Ptr (RIP.Complex RIP.CChar)
global_complex_char =
  RIP.unsafePerformIO hs_bindgen_7c8cb41a7febc6fc
