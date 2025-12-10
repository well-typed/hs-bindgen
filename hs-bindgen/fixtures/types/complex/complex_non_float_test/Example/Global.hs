{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/complex_non_float_test.h>"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short _Complex *hs_bindgen_f06a1feab15d0572 (void)"
  , "{"
  , "  return &global_complex_unsigned_short;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_short_ptr */"
  , "__attribute__ ((const))"
  , "signed short _Complex *hs_bindgen_96f1e350c6c42760 (void)"
  , "{"
  , "  return &global_complex_short;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int _Complex *hs_bindgen_f11054676f537692 (void)"
  , "{"
  , "  return &global_complex_unsigned_int;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_int_ptr */"
  , "__attribute__ ((const))"
  , "signed int _Complex *hs_bindgen_c5a5feb6d6df39b6 (void)"
  , "{"
  , "  return &global_complex_int;"
  , "}"
  , "/* test_typescomplexcomplex_non_floa_Example_get_global_complex_char_ptr */"
  , "__attribute__ ((const))"
  , "char _Complex *hs_bindgen_4265dc893a126b40 (void)"
  , "{"
  , "  return &global_complex_char;"
  , "}"
  ]))

-- | __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_short_ptr@
foreign import ccall unsafe "hs_bindgen_f06a1feab15d0572" hs_bindgen_f06a1feab15d0572 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))

{-# NOINLINE global_complex_unsigned_short_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:3:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUShort)
global_complex_unsigned_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f06a1feab15d0572

-- | __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_short_ptr@
foreign import ccall unsafe "hs_bindgen_96f1e350c6c42760" hs_bindgen_96f1e350c6c42760 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))

{-# NOINLINE global_complex_short_ptr #-}

{-| __C declaration:__ @global_complex_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:4:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CShort)
global_complex_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_96f1e350c6c42760

-- | __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_unsigned_int_ptr@
foreign import ccall unsafe "hs_bindgen_f11054676f537692" hs_bindgen_f11054676f537692 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))

{-# NOINLINE global_complex_unsigned_int_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:5:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUInt)
global_complex_unsigned_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f11054676f537692

-- | __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_int_ptr@
foreign import ccall unsafe "hs_bindgen_c5a5feb6d6df39b6" hs_bindgen_c5a5feb6d6df39b6 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))

{-# NOINLINE global_complex_int_ptr #-}

{-| __C declaration:__ @global_complex_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:6:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CInt)
global_complex_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c5a5feb6d6df39b6

-- | __unique:__ @test_typescomplexcomplex_non_floa_Example_get_global_complex_char_ptr@
foreign import ccall unsafe "hs_bindgen_4265dc893a126b40" hs_bindgen_4265dc893a126b40 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))

{-# NOINLINE global_complex_char_ptr #-}

{-| __C declaration:__ @global_complex_char@

    __defined at:__ @types\/complex\/complex_non_float_test.h:7:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_char_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CChar)
global_complex_char_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4265dc893a126b40
