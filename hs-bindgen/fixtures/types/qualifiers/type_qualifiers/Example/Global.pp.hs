{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/qualifiers/type_qualifiers.h>"
  , "/* get_a_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* get_b_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* get_c_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* get_d_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107 (void)"
  , "{"
  , "  return &d;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd" hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr FC.CInt)
    )

hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd_base

{-# NOINLINE a_ptr #-}

{-| __C declaration:__ @a@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:5:18@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
a_ptr :: Ptr.Ptr FC.CInt
a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_3afcbd8536cf21bd

{-# NOINLINE a #-}

a :: FC.CInt
a = GHC.IO.Unsafe.unsafePerformIO (F.peek a_ptr)

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee" hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
    )

hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee_base

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:7:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
b_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_fcd0c984d664f6ee

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef" hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
    )

hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef_base

{-# NOINLINE c_ptr #-}

{-| __C declaration:__ @c@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:9:19@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
c_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
c_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_d61ea07e27589aef

{-# NOINLINE c #-}

c :: Ptr.Ptr FC.CInt
c = GHC.IO.Unsafe.unsafePerformIO (F.peek c_ptr)

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107" hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
    )

hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107_base

{-# NOINLINE d_ptr #-}

{-| __C declaration:__ @d@

    __defined at:__ @types\/qualifiers\/type_qualifiers.h:11:25@

    __exported by:__ @types\/qualifiers\/type_qualifiers.h@
-}
d_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
d_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesqualifierstype_qualifie_d1d6489b06a70107

{-# NOINLINE d #-}

d :: Ptr.Ptr FC.CInt
d = GHC.IO.Unsafe.unsafePerformIO (F.peek d_ptr)
