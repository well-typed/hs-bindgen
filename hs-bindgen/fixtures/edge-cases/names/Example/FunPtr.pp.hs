{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "/* get_by_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8 (void)) (void)"
  , "{"
  , "  return &by;"
  , "}"
  , "/* get_forall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_a56841d4692515a9 (void)) (void)"
  , "{"
  , "  return &forall;"
  , "}"
  , "/* get_mdo_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378 (void)) (void)"
  , "{"
  , "  return &mdo;"
  , "}"
  , "/* get_pattern_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_b393d3c39de80903 (void)) (void)"
  , "{"
  , "  return &pattern;"
  , "}"
  , "/* get_proc_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_f73545ae9224ff2d (void)) (void)"
  , "{"
  , "  return &proc;"
  , "}"
  , "/* get_rec_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_0dd0f0805657312b (void)) (void)"
  , "{"
  , "  return &rec;"
  , "}"
  , "/* get_using_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037 (void)) (void)"
  , "{"
  , "  return &using;"
  , "}"
  , "/* get_anyclass_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4 (void)) (void)"
  , "{"
  , "  return &anyclass;"
  , "}"
  , "/* get_capi_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_3513093206483c6f (void)) (void)"
  , "{"
  , "  return &capi;"
  , "}"
  , "/* get_cases_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9 (void)) (void)"
  , "{"
  , "  return &cases;"
  , "}"
  , "/* get_ccall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_79b6583362113331 (void)) (void)"
  , "{"
  , "  return &ccall;"
  , "}"
  , "/* get_dynamic_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45 (void)) (void)"
  , "{"
  , "  return &dynamic;"
  , "}"
  , "/* get_export_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_98505ac7a8664e29 (void)) (void)"
  , "{"
  , "  return &export;"
  , "}"
  , "/* get_family_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_849e77496a12f443 (void)) (void)"
  , "{"
  , "  return &family;"
  , "}"
  , "/* get_group_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5 (void)) (void)"
  , "{"
  , "  return &group;"
  , "}"
  , "/* get_interruptible_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_d016728709223884 (void)) (void)"
  , "{"
  , "  return &interruptible;"
  , "}"
  , "/* get_javascript_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_fa5819996081f2b8 (void)) (void)"
  , "{"
  , "  return &javascript;"
  , "}"
  , "/* get_label_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_0004d966bf5f8027 (void)) (void)"
  , "{"
  , "  return &label;"
  , "}"
  , "/* get_prim_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea (void)) (void)"
  , "{"
  , "  return &prim;"
  , "}"
  , "/* get_role_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_89449e7f8f90fc97 (void)) (void)"
  , "{"
  , "  return &role;"
  , "}"
  , "/* get_safe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_983c057a9645fd29 (void)) (void)"
  , "{"
  , "  return &safe;"
  , "}"
  , "/* get_stdcall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_8ea4a9581868a08c (void)) (void)"
  , "{"
  , "  return &stdcall;"
  , "}"
  , "/* get_stock_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_9d4197174428177d (void)) (void)"
  , "{"
  , "  return &stock;"
  , "}"
  , "/* get_unsafe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_6eb48adc74567d6d (void)) (void)"
  , "{"
  , "  return &unsafe;"
  , "}"
  , "/* get_via_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_64eddb721da8c268 (void)) (void)"
  , "{"
  , "  return &via;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8" hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8_base

{-# NOINLINE by'_ptr #-}

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by'_ptr :: Ptr.FunPtr (IO ())
by'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_3e566432e1b7bdd8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_a56841d4692515a9" hs_bindgen_test_edgecasesnames_a56841d4692515a9_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_a56841d4692515a9 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_a56841d4692515a9 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_a56841d4692515a9_base

{-# NOINLINE forall'_ptr #-}

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall'_ptr :: Ptr.FunPtr (IO ())
forall'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_a56841d4692515a9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378" hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378_base

{-# NOINLINE mdo'_ptr #-}

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo'_ptr :: Ptr.FunPtr (IO ())
mdo'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_4b14baa1fbc8f378

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_b393d3c39de80903" hs_bindgen_test_edgecasesnames_b393d3c39de80903_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_b393d3c39de80903 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_b393d3c39de80903 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_b393d3c39de80903_base

{-# NOINLINE pattern'_ptr #-}

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern'_ptr :: Ptr.FunPtr (IO ())
pattern'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_b393d3c39de80903

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_f73545ae9224ff2d" hs_bindgen_test_edgecasesnames_f73545ae9224ff2d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_f73545ae9224ff2d ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_f73545ae9224ff2d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_f73545ae9224ff2d_base

{-# NOINLINE proc'_ptr #-}

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc'_ptr :: Ptr.FunPtr (IO ())
proc'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_f73545ae9224ff2d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0dd0f0805657312b" hs_bindgen_test_edgecasesnames_0dd0f0805657312b_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_0dd0f0805657312b ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_0dd0f0805657312b =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_0dd0f0805657312b_base

{-# NOINLINE rec'_ptr #-}

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec'_ptr :: Ptr.FunPtr (IO ())
rec'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_0dd0f0805657312b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037" hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037_base

{-# NOINLINE using'_ptr #-}

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using'_ptr :: Ptr.FunPtr (IO ())
using'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_aa3ce68f2bc5f037

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4" hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4_base

{-# NOINLINE anyclass_ptr #-}

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass_ptr :: Ptr.FunPtr (IO ())
anyclass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_9f90f5dd46a351a4

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_3513093206483c6f" hs_bindgen_test_edgecasesnames_3513093206483c6f_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_3513093206483c6f ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_3513093206483c6f =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_3513093206483c6f_base

{-# NOINLINE capi_ptr #-}

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi_ptr :: Ptr.FunPtr (IO ())
capi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_3513093206483c6f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9" hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9_base

{-# NOINLINE cases_ptr #-}

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases_ptr :: Ptr.FunPtr (IO ())
cases_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_af7d0bdd6562f7d9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_79b6583362113331" hs_bindgen_test_edgecasesnames_79b6583362113331_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_79b6583362113331 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_79b6583362113331 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_79b6583362113331_base

{-# NOINLINE ccall_ptr #-}

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall_ptr :: Ptr.FunPtr (IO ())
ccall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_79b6583362113331

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45" hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45_base

{-# NOINLINE dynamic_ptr #-}

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic_ptr :: Ptr.FunPtr (IO ())
dynamic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_5c19071f2f4eaf45

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_98505ac7a8664e29" hs_bindgen_test_edgecasesnames_98505ac7a8664e29_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_98505ac7a8664e29 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_98505ac7a8664e29 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_98505ac7a8664e29_base

{-# NOINLINE export_ptr #-}

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export_ptr :: Ptr.FunPtr (IO ())
export_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_98505ac7a8664e29

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_849e77496a12f443" hs_bindgen_test_edgecasesnames_849e77496a12f443_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_849e77496a12f443 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_849e77496a12f443 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_849e77496a12f443_base

{-# NOINLINE family_ptr #-}

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family_ptr :: Ptr.FunPtr (IO ())
family_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_849e77496a12f443

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5" hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5_base

{-# NOINLINE group_ptr #-}

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group_ptr :: Ptr.FunPtr (IO ())
group_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_41e72434cbcfa0d5

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_d016728709223884" hs_bindgen_test_edgecasesnames_d016728709223884_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_d016728709223884 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_d016728709223884 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_d016728709223884_base

{-# NOINLINE interruptible_ptr #-}

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible_ptr :: Ptr.FunPtr (IO ())
interruptible_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_d016728709223884

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_fa5819996081f2b8" hs_bindgen_test_edgecasesnames_fa5819996081f2b8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_fa5819996081f2b8 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_fa5819996081f2b8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_fa5819996081f2b8_base

{-# NOINLINE javascript_ptr #-}

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript_ptr :: Ptr.FunPtr (IO ())
javascript_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_fa5819996081f2b8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_0004d966bf5f8027" hs_bindgen_test_edgecasesnames_0004d966bf5f8027_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_0004d966bf5f8027 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_0004d966bf5f8027 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_0004d966bf5f8027_base

{-# NOINLINE label_ptr #-}

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label_ptr :: Ptr.FunPtr (IO ())
label_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_0004d966bf5f8027

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea" hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea_base

{-# NOINLINE prim_ptr #-}

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim_ptr :: Ptr.FunPtr (IO ())
prim_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_d9ec78c543d7f0ea

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_89449e7f8f90fc97" hs_bindgen_test_edgecasesnames_89449e7f8f90fc97_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_89449e7f8f90fc97 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_89449e7f8f90fc97 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_89449e7f8f90fc97_base

{-# NOINLINE role_ptr #-}

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role_ptr :: Ptr.FunPtr (IO ())
role_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_89449e7f8f90fc97

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_983c057a9645fd29" hs_bindgen_test_edgecasesnames_983c057a9645fd29_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_983c057a9645fd29 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_983c057a9645fd29 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_983c057a9645fd29_base

{-# NOINLINE safe_ptr #-}

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe_ptr :: Ptr.FunPtr (IO ())
safe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_983c057a9645fd29

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_8ea4a9581868a08c" hs_bindgen_test_edgecasesnames_8ea4a9581868a08c_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_8ea4a9581868a08c ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_8ea4a9581868a08c =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_8ea4a9581868a08c_base

{-# NOINLINE stdcall_ptr #-}

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall_ptr :: Ptr.FunPtr (IO ())
stdcall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_8ea4a9581868a08c

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_9d4197174428177d" hs_bindgen_test_edgecasesnames_9d4197174428177d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_9d4197174428177d ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_9d4197174428177d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_9d4197174428177d_base

{-# NOINLINE stock_ptr #-}

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock_ptr :: Ptr.FunPtr (IO ())
stock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_9d4197174428177d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_6eb48adc74567d6d" hs_bindgen_test_edgecasesnames_6eb48adc74567d6d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_6eb48adc74567d6d ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_6eb48adc74567d6d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_6eb48adc74567d6d_base

{-# NOINLINE unsafe_ptr #-}

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe_ptr :: Ptr.FunPtr (IO ())
unsafe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_6eb48adc74567d6d

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_64eddb721da8c268" hs_bindgen_test_edgecasesnames_64eddb721da8c268_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (IO ()))
    )

hs_bindgen_test_edgecasesnames_64eddb721da8c268 ::
     IO (Ptr.FunPtr (IO ()))
hs_bindgen_test_edgecasesnames_64eddb721da8c268 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_edgecasesnames_64eddb721da8c268_base

{-# NOINLINE via_ptr #-}

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via_ptr :: Ptr.FunPtr (IO ())
via_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_64eddb721da8c268
