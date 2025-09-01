{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(CAPI.addCSource "#include <type_qualifiers.h>\n/* get_a_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_type_qualifiers_be05c6870fad4f33 (void) { return &a; } \n/* get_b_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_type_qualifiers_401ecb7e80957164 (void) { return &b; } \n/* get_c_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_type_qualifiers_0b370289c6c19db4 (void) { return &c; } \n/* get_d_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_type_qualifiers_cc41ed0d2b848565 (void) { return &d; } \n_Bool hs_bindgen_test_type_qualifiers_9d6d039971edcd60 (char const **arg1, size_t arg2) { return list_example(arg1, arg2); }\n/* get_list_example_ptr */ __attribute__ ((const)) _Bool (*hs_bindgen_test_type_qualifiers_c40a51053a97fb29 (void)) (char const **arg1, size_t arg2) { return &list_example; } \n")

foreign import ccall unsafe "hs_bindgen_test_type_qualifiers_be05c6870fad4f33" hs_bindgen_test_type_qualifiers_be05c6870fad4f33
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE a_ptr #-}

a_ptr :: F.Ptr FC.CInt
a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_type_qualifiers_be05c6870fad4f33

{-# NOINLINE a #-}

a :: FC.CInt
a = GHC.IO.Unsafe.unsafePerformIO (F.peek a_ptr)

foreign import ccall unsafe "hs_bindgen_test_type_qualifiers_401ecb7e80957164" hs_bindgen_test_type_qualifiers_401ecb7e80957164
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE b_ptr #-}

b_ptr :: F.Ptr (F.Ptr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_type_qualifiers_401ecb7e80957164

foreign import ccall unsafe "hs_bindgen_test_type_qualifiers_0b370289c6c19db4" hs_bindgen_test_type_qualifiers_0b370289c6c19db4
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE c_ptr #-}

c_ptr :: F.Ptr (F.Ptr FC.CInt)
c_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_type_qualifiers_0b370289c6c19db4

{-# NOINLINE c #-}

c :: F.Ptr FC.CInt
c = GHC.IO.Unsafe.unsafePerformIO (F.peek c_ptr)

foreign import ccall unsafe "hs_bindgen_test_type_qualifiers_cc41ed0d2b848565" hs_bindgen_test_type_qualifiers_cc41ed0d2b848565
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE d_ptr #-}

d_ptr :: F.Ptr (F.Ptr FC.CInt)
d_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_type_qualifiers_cc41ed0d2b848565

{-# NOINLINE d #-}

d :: F.Ptr FC.CInt
d = GHC.IO.Unsafe.unsafePerformIO (F.peek d_ptr)

{-| __from C:__ @list_example@ -}
foreign import ccall safe "hs_bindgen_test_type_qualifiers_9d6d039971edcd60" list_example
  :: F.Ptr (F.Ptr FC.CChar)
     {- ^ __from C:__ @items@ -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __from C:__ @count@ -}
  -> IO FC.CBool

foreign import ccall unsafe "hs_bindgen_test_type_qualifiers_c40a51053a97fb29" hs_bindgen_test_type_qualifiers_c40a51053a97fb29
  :: IO (F.FunPtr ((F.Ptr (F.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool))

{-# NOINLINE list_example_ptr #-}

list_example_ptr :: F.FunPtr ((F.Ptr (F.Ptr FC.CChar)) -> HsBindgen.Runtime.Prelude.CSize -> IO FC.CBool)
list_example_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_type_qualifiers_c40a51053a97fb29
