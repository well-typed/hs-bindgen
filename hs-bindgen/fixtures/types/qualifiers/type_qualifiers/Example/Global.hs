{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* Example_get_a_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_typesqualifierstype_qualifie_9a8725101f8eced3 (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* Example_get_b_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_test_typesqualifierstype_qualifie_73b81ddc3378bde8 (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* Example_get_c_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_test_typesqualifierstype_qualifie_7d5559f3855dc35a (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* Example_get_d_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_test_typesqualifierstype_qualifie_7bcac2f258a3b53b (void)"
  , "{"
  , "  return &d;"
  , "}"
  ]))

{-| __unique:__ @Example_get_a_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_9a8725101f8eced3" hs_bindgen_test_typesqualifierstype_qualifie_9a8725101f8eced3 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE a_ptr #-}

{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
a_ptr :: Ptr.Ptr FC.CInt
a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_9a8725101f8eced3

{-# NOINLINE a #-}

a :: FC.CInt
a = GHC.IO.Unsafe.unsafePerformIO (F.peek a_ptr)

{-| __unique:__ @Example_get_b_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_73b81ddc3378bde8" hs_bindgen_test_typesqualifierstype_qualifie_73b81ddc3378bde8 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_73b81ddc3378bde8

{-| __unique:__ @Example_get_c_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_7d5559f3855dc35a" hs_bindgen_test_typesqualifierstype_qualifie_7d5559f3855dc35a ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE c_ptr #-}

{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
c_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
c_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_7d5559f3855dc35a

{-# NOINLINE c #-}

c :: Ptr.Ptr FC.CInt
c = GHC.IO.Unsafe.unsafePerformIO (F.peek c_ptr)

{-| __unique:__ @Example_get_d_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_7bcac2f258a3b53b" hs_bindgen_test_typesqualifierstype_qualifie_7bcac2f258a3b53b ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE d_ptr #-}

{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
d_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
d_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_7bcac2f258a3b53b

{-# NOINLINE d #-}

d :: Ptr.Ptr FC.CInt
d = GHC.IO.Unsafe.unsafePerformIO (F.peek d_ptr)
