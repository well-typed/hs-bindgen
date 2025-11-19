{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/complex_non_float_test.h>"
  , "/* get_global_complex_unsigned_short_ptr */"
  , "__attribute__ ((const))"
  , "unsigned short _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62 (void)"
  , "{"
  , "  return &global_complex_unsigned_short;"
  , "}"
  , "/* get_global_complex_short_ptr */"
  , "__attribute__ ((const))"
  , "signed short _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91 (void)"
  , "{"
  , "  return &global_complex_short;"
  , "}"
  , "/* get_global_complex_unsigned_int_ptr */"
  , "__attribute__ ((const))"
  , "unsigned int _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c (void)"
  , "{"
  , "  return &global_complex_unsigned_int;"
  , "}"
  , "/* get_global_complex_int_ptr */"
  , "__attribute__ ((const))"
  , "signed int _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969 (void)"
  , "{"
  , "  return &global_complex_int;"
  , "}"
  , "/* get_global_complex_char_ptr */"
  , "__attribute__ ((const))"
  , "char _Complex *hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69 (void)"
  , "{"
  , "  return &global_complex_char;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62" hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))
    )

hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))
hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62_base

{-# NOINLINE global_complex_unsigned_short_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:3:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUShort)
global_complex_unsigned_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_1a47f4ebfee55a62

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91" hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))
    )

hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))
hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91_base

{-# NOINLINE global_complex_short_ptr #-}

{-| __C declaration:__ @global_complex_short@

    __defined at:__ @types\/complex\/complex_non_float_test.h:4:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CShort)
global_complex_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_d56d4f7328166c91

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c" hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))
    )

hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))
hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c_base

{-# NOINLINE global_complex_unsigned_int_ptr #-}

{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:5:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_unsigned_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUInt)
global_complex_unsigned_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_b596fcc6ded5636c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969" hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))
    )

hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))
hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969_base

{-# NOINLINE global_complex_int_ptr #-}

{-| __C declaration:__ @global_complex_int@

    __defined at:__ @types\/complex\/complex_non_float_test.h:6:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CInt)
global_complex_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_9f8a73e0d4ba6969

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69" hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))
    )

hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))
hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69_base

{-# NOINLINE global_complex_char_ptr #-}

{-| __C declaration:__ @global_complex_char@

    __defined at:__ @types\/complex\/complex_non_float_test.h:7:32@

    __exported by:__ @types\/complex\/complex_non_float_test.h@
-}
global_complex_char_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CChar)
global_complex_char_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexcomplex_non_floa_4727b3aff4118d69
