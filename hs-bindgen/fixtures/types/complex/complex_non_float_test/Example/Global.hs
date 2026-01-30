{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Prelude (IO)

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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short@
hs_bindgen_f3018b1e87470bdd :: IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))
hs_bindgen_f3018b1e87470bdd =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_f3018b1e87470bdd_base

{-# NOINLINE global_complex_unsigned_short #-}
{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h 3:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_short :: Ptr.Ptr (Data.Complex.Complex FC.CUShort)
global_complex_unsigned_short =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3018b1e87470bdd

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_short@
foreign import ccall unsafe "hs_bindgen_a553be84170a3efa" hs_bindgen_a553be84170a3efa_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_short@
hs_bindgen_a553be84170a3efa :: IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))
hs_bindgen_a553be84170a3efa =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_a553be84170a3efa_base

{-# NOINLINE global_complex_short #-}
{-| __C declaration:__ @global_complex_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h 4:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_short :: Ptr.Ptr (Data.Complex.Complex FC.CShort)
global_complex_short =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a553be84170a3efa

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int@
foreign import ccall unsafe "hs_bindgen_072fc08ef6d9395a" hs_bindgen_072fc08ef6d9395a_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int@
hs_bindgen_072fc08ef6d9395a :: IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))
hs_bindgen_072fc08ef6d9395a =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_072fc08ef6d9395a_base

{-# NOINLINE global_complex_unsigned_int #-}
{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h 5:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_int :: Ptr.Ptr (Data.Complex.Complex FC.CUInt)
global_complex_unsigned_int =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_072fc08ef6d9395a

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_int@
foreign import ccall unsafe "hs_bindgen_1199a9d54fe334cb" hs_bindgen_1199a9d54fe334cb_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_int@
hs_bindgen_1199a9d54fe334cb :: IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))
hs_bindgen_1199a9d54fe334cb =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1199a9d54fe334cb_base

{-# NOINLINE global_complex_int #-}
{-| __C declaration:__ @global_complex_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h 6:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_int :: Ptr.Ptr (Data.Complex.Complex FC.CInt)
global_complex_int =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1199a9d54fe334cb

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_char@
foreign import ccall unsafe "hs_bindgen_7c8cb41a7febc6fc" hs_bindgen_7c8cb41a7febc6fc_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_char@
hs_bindgen_7c8cb41a7febc6fc :: IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))
hs_bindgen_7c8cb41a7febc6fc =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_7c8cb41a7febc6fc_base

{-# NOINLINE global_complex_char #-}
{-| __C declaration:__ @global_complex_char@

    __defined at:__ @types\/complex\/complex_non_float_test.h 7:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_char :: Ptr.Ptr (Data.Complex.Complex FC.CChar)
global_complex_char =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7c8cb41a7febc6fc
