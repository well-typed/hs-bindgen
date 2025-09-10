{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <complex_non_float_test.h>\n/* get_global_complex_unsigned_short_ptr */ __attribute__ ((const)) unsigned short _Complex *hs_bindgen_test_complex_non_float_test_86728dd7dddedb74 (void) { return &global_complex_unsigned_short; } \n/* get_global_complex_short_ptr */ __attribute__ ((const)) signed short _Complex *hs_bindgen_test_complex_non_float_test_7c9be84d0b1e5916 (void) { return &global_complex_short; } \n/* get_global_complex_unsigned_int_ptr */ __attribute__ ((const)) unsigned int _Complex *hs_bindgen_test_complex_non_float_test_f2f0bb5bb1be7a50 (void) { return &global_complex_unsigned_int; } \n/* get_global_complex_int_ptr */ __attribute__ ((const)) signed int _Complex *hs_bindgen_test_complex_non_float_test_4c313edb894be30a (void) { return &global_complex_int; } \n/* get_global_complex_char_ptr */ __attribute__ ((const)) char _Complex *hs_bindgen_test_complex_non_float_test_983de9835e8954ae (void) { return &global_complex_char; } \n")

{-| __C declaration:__ @global_complex_unsigned_short@

    __defined at:__ @complex_non_float_test.h:3:32@

    __exported by:__ @complex_non_float_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_non_float_test_86728dd7dddedb74" hs_bindgen_test_complex_non_float_test_86728dd7dddedb74
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CUShort))

{-# NOINLINE global_complex_unsigned_short_ptr #-}

global_complex_unsigned_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUShort)
global_complex_unsigned_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_non_float_test_86728dd7dddedb74

{-| __C declaration:__ @global_complex_short@

    __defined at:__ @complex_non_float_test.h:4:32@

    __exported by:__ @complex_non_float_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_non_float_test_7c9be84d0b1e5916" hs_bindgen_test_complex_non_float_test_7c9be84d0b1e5916
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CShort))

{-# NOINLINE global_complex_short_ptr #-}

global_complex_short_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CShort)
global_complex_short_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_non_float_test_7c9be84d0b1e5916

{-| __C declaration:__ @global_complex_unsigned_int@

    __defined at:__ @complex_non_float_test.h:5:32@

    __exported by:__ @complex_non_float_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_non_float_test_f2f0bb5bb1be7a50" hs_bindgen_test_complex_non_float_test_f2f0bb5bb1be7a50
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CUInt))

{-# NOINLINE global_complex_unsigned_int_ptr #-}

global_complex_unsigned_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CUInt)
global_complex_unsigned_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_non_float_test_f2f0bb5bb1be7a50

{-| __C declaration:__ @global_complex_int@

    __defined at:__ @complex_non_float_test.h:6:32@

    __exported by:__ @complex_non_float_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_non_float_test_4c313edb894be30a" hs_bindgen_test_complex_non_float_test_4c313edb894be30a
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CInt))

{-# NOINLINE global_complex_int_ptr #-}

global_complex_int_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CInt)
global_complex_int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_non_float_test_4c313edb894be30a

{-| __C declaration:__ @global_complex_char@

    __defined at:__ @complex_non_float_test.h:7:32@

    __exported by:__ @complex_non_float_test.h@
-}
foreign import ccall unsafe "hs_bindgen_test_complex_non_float_test_983de9835e8954ae" hs_bindgen_test_complex_non_float_test_983de9835e8954ae
  :: IO (Ptr.Ptr (Data.Complex.Complex FC.CChar))

{-# NOINLINE global_complex_char_ptr #-}

global_complex_char_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CChar)
global_complex_char_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_complex_non_float_test_983de9835e8954ae
