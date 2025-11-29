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
  , "/* Example_get_global_complex_unsigned_short_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_6d55f1ca3a2499b8 (void)"
  , "{"
  , "  return &global_complex_unsigned_short;"
  , "}"
  , "/* Example_get_global_complex_short_ptr */"
  , "__attribute__ ((const))"
  , "signed short _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_2d1151e8eeef7711 (void)"
  , "{"
  , "  return &global_complex_short;"
  , "}"
  , "/* Example_get_global_complex_unsigned_int_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_93efc31bade3ca7a (void)"
  , "{"
  , "  return &global_complex_unsigned_int;"
  , "}"
  , "/* Example_get_global_complex_int_ptr */"
  , "__attribute__ ((const))"
  , "signed int _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_fadd2d069427cf8b (void)"
  , "{"
  , "  return &global_complex_int;"
  , "}"
  , "/* Example_get_global_complex_char_ptr */"
  , "__attribute__ ((const))"
  , "char _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_25f2b5af5fdd8414 (void)"
  , "{"
  , "  return &global_complex_char;"
  , "}"
  ]))

{-| __unique:__ @Example_get_global_complex_unsigned_short_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_6d55f1ca3a2499b8" hs_bindgen_test_typescomplexcomplex_non_floa_6d55f1ca3a2499b8 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))

{-# NOINLINE global_complex_unsigned_short_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:3:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUShort)
global_complex_unsigned_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_6d55f1ca3a2499b8

{-| __unique:__ @Example_get_global_complex_short_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_2d1151e8eeef7711" hs_bindgen_test_typescomplexcomplex_non_floa_2d1151e8eeef7711 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))

{-# NOINLINE global_complex_short_ptr #-}

{-| __C declaration:__ @global_complex_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:4:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CShort)
global_complex_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_2d1151e8eeef7711

{-| __unique:__ @Example_get_global_complex_unsigned_int_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_93efc31bade3ca7a" hs_bindgen_test_typescomplexcomplex_non_floa_93efc31bade3ca7a ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))

{-# NOINLINE global_complex_unsigned_int_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:5:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUInt)
global_complex_unsigned_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_93efc31bade3ca7a

{-| __unique:__ @Example_get_global_complex_int_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_fadd2d069427cf8b" hs_bindgen_test_typescomplexcomplex_non_floa_fadd2d069427cf8b ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))

{-# NOINLINE global_complex_int_ptr #-}

{-| __C declaration:__ @global_complex_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:6:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CInt)
global_complex_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_fadd2d069427cf8b

{-| __unique:__ @Example_get_global_complex_char_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_25f2b5af5fdd8414" hs_bindgen_test_typescomplexcomplex_non_floa_25f2b5af5fdd8414 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))

{-# NOINLINE global_complex_char_ptr #-}

{-| __C declaration:__ @global_complex_char@

    __defined at:__ @types\/complex\/complex_non_float_test.h:7:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_char_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CChar)
global_complex_char_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_25f2b5af5fdd8414
