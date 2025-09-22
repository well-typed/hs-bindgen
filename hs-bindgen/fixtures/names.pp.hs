{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <names.h>\nvoid hs_bindgen_test_names_0e7f535b7085fc20 (void) { by(); }\nvoid hs_bindgen_test_names_fc72c248490e573d (void) { forall(); }\nvoid hs_bindgen_test_names_f21dd25a8b27374a (void) { mdo(); }\nvoid hs_bindgen_test_names_0ab5fb5a8105f789 (void) { pattern(); }\nvoid hs_bindgen_test_names_c14da8252137427c (void) { proc(); }\nvoid hs_bindgen_test_names_1706dcb069f095bc (void) { rec(); }\nvoid hs_bindgen_test_names_9cbe86d126f1d8ee (void) { using(); }\nvoid hs_bindgen_test_names_84e650893f3a92e0 (void) { anyclass(); }\nvoid hs_bindgen_test_names_a931e0b3ecf47315 (void) { capi(); }\nvoid hs_bindgen_test_names_8a21f58b60402b4c (void) { cases(); }\nvoid hs_bindgen_test_names_e72f0a6b1520490f (void) { ccall(); }\nvoid hs_bindgen_test_names_87e085120fafde48 (void) { dynamic(); }\nvoid hs_bindgen_test_names_0e6d6a351c0f2d6b (void) { export(); }\nvoid hs_bindgen_test_names_bd4d89d0e474ae7a (void) { family(); }\nvoid hs_bindgen_test_names_d33488d9cb279feb (void) { group(); }\nvoid hs_bindgen_test_names_9a3851f5a39a9e5d (void) { interruptible(); }\nvoid hs_bindgen_test_names_fa9100d6d4da2bd4 (void) { javascript(); }\nvoid hs_bindgen_test_names_264e9aa826f1d8fd (void) { label(); }\nvoid hs_bindgen_test_names_06aa327310da5120 (void) { prim(); }\nvoid hs_bindgen_test_names_0c2d88e0ce229900 (void) { role(); }\nvoid hs_bindgen_test_names_a703e7f54db3712e (void) { safe(); }\nvoid hs_bindgen_test_names_001ca64746b969f3 (void) { stdcall(); }\nvoid hs_bindgen_test_names_44ab69657e70c20b (void) { stock(); }\nvoid hs_bindgen_test_names_cf74a7d7f3568406 (void) { unsafe(); }\nvoid hs_bindgen_test_names_f24fb703d4751a60 (void) { via(); }\n/* get_by_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_3e566432e1b7bdd8 (void)) (void) { return &by; } \n/* get_forall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_a56841d4692515a9 (void)) (void) { return &forall; } \n/* get_mdo_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_4b14baa1fbc8f378 (void)) (void) { return &mdo; } \n/* get_pattern_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_b393d3c39de80903 (void)) (void) { return &pattern; } \n/* get_proc_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_f73545ae9224ff2d (void)) (void) { return &proc; } \n/* get_rec_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_0dd0f0805657312b (void)) (void) { return &rec; } \n/* get_using_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_aa3ce68f2bc5f037 (void)) (void) { return &using; } \n/* get_anyclass_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_9f90f5dd46a351a4 (void)) (void) { return &anyclass; } \n/* get_capi_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_3513093206483c6f (void)) (void) { return &capi; } \n/* get_cases_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_af7d0bdd6562f7d9 (void)) (void) { return &cases; } \n/* get_ccall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_79b6583362113331 (void)) (void) { return &ccall; } \n/* get_dynamic_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_5c19071f2f4eaf45 (void)) (void) { return &dynamic; } \n/* get_export_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_98505ac7a8664e29 (void)) (void) { return &export; } \n/* get_family_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_849e77496a12f443 (void)) (void) { return &family; } \n/* get_group_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_41e72434cbcfa0d5 (void)) (void) { return &group; } \n/* get_interruptible_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_d016728709223884 (void)) (void) { return &interruptible; } \n/* get_javascript_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_fa5819996081f2b8 (void)) (void) { return &javascript; } \n/* get_label_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_0004d966bf5f8027 (void)) (void) { return &label; } \n/* get_prim_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_d9ec78c543d7f0ea (void)) (void) { return &prim; } \n/* get_role_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_89449e7f8f90fc97 (void)) (void) { return &role; } \n/* get_safe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_983c057a9645fd29 (void)) (void) { return &safe; } \n/* get_stdcall_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_8ea4a9581868a08c (void)) (void) { return &stdcall; } \n/* get_stock_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_9d4197174428177d (void)) (void) { return &stock; } \n/* get_unsafe_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_6eb48adc74567d6d (void)) (void) { return &unsafe; } \n/* get_via_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_names_64eddb721da8c268 (void)) (void) { return &via; } \n")

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0e7f535b7085fc20" by'
  :: IO ()

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_fc72c248490e573d" forall'
  :: IO ()

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_f21dd25a8b27374a" mdo'
  :: IO ()

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0ab5fb5a8105f789" pattern'
  :: IO ()

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_c14da8252137427c" proc'
  :: IO ()

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_1706dcb069f095bc" rec'
  :: IO ()

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9cbe86d126f1d8ee" using'
  :: IO ()

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_84e650893f3a92e0" anyclass
  :: IO ()

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a931e0b3ecf47315" capi
  :: IO ()

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_8a21f58b60402b4c" cases
  :: IO ()

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_e72f0a6b1520490f" ccall
  :: IO ()

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_87e085120fafde48" dynamic
  :: IO ()

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0e6d6a351c0f2d6b" export
  :: IO ()

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_bd4d89d0e474ae7a" family
  :: IO ()

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_d33488d9cb279feb" group
  :: IO ()

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_9a3851f5a39a9e5d" interruptible
  :: IO ()

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_fa9100d6d4da2bd4" javascript
  :: IO ()

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_264e9aa826f1d8fd" label
  :: IO ()

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_06aa327310da5120" prim
  :: IO ()

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_0c2d88e0ce229900" role
  :: IO ()

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_a703e7f54db3712e" safe
  :: IO ()

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_001ca64746b969f3" stdcall
  :: IO ()

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_44ab69657e70c20b" stock
  :: IO ()

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_cf74a7d7f3568406" unsafe
  :: IO ()

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall safe "hs_bindgen_test_names_f24fb703d4751a60" via
  :: IO ()

{-| __C declaration:__ @by@

    __defined at:__ @names.h:3:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_3e566432e1b7bdd8" hs_bindgen_test_names_3e566432e1b7bdd8
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE by'_ptr #-}

by'_ptr :: Ptr.FunPtr (IO ())
by'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_3e566432e1b7bdd8

{-| __C declaration:__ @forall@

    __defined at:__ @names.h:4:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_a56841d4692515a9" hs_bindgen_test_names_a56841d4692515a9
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE forall'_ptr #-}

forall'_ptr :: Ptr.FunPtr (IO ())
forall'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_a56841d4692515a9

{-| __C declaration:__ @mdo@

    __defined at:__ @names.h:5:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_4b14baa1fbc8f378" hs_bindgen_test_names_4b14baa1fbc8f378
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE mdo'_ptr #-}

mdo'_ptr :: Ptr.FunPtr (IO ())
mdo'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_4b14baa1fbc8f378

{-| __C declaration:__ @pattern@

    __defined at:__ @names.h:6:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_b393d3c39de80903" hs_bindgen_test_names_b393d3c39de80903
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE pattern'_ptr #-}

pattern'_ptr :: Ptr.FunPtr (IO ())
pattern'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_b393d3c39de80903

{-| __C declaration:__ @proc@

    __defined at:__ @names.h:7:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_f73545ae9224ff2d" hs_bindgen_test_names_f73545ae9224ff2d
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE proc'_ptr #-}

proc'_ptr :: Ptr.FunPtr (IO ())
proc'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_f73545ae9224ff2d

{-| __C declaration:__ @rec@

    __defined at:__ @names.h:8:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_0dd0f0805657312b" hs_bindgen_test_names_0dd0f0805657312b
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE rec'_ptr #-}

rec'_ptr :: Ptr.FunPtr (IO ())
rec'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_0dd0f0805657312b

{-| __C declaration:__ @using@

    __defined at:__ @names.h:9:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_aa3ce68f2bc5f037" hs_bindgen_test_names_aa3ce68f2bc5f037
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE using'_ptr #-}

using'_ptr :: Ptr.FunPtr (IO ())
using'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_aa3ce68f2bc5f037

{-| __C declaration:__ @anyclass@

    __defined at:__ @names.h:12:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_9f90f5dd46a351a4" hs_bindgen_test_names_9f90f5dd46a351a4
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE anyclass_ptr #-}

anyclass_ptr :: Ptr.FunPtr (IO ())
anyclass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_9f90f5dd46a351a4

{-| __C declaration:__ @capi@

    __defined at:__ @names.h:13:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_3513093206483c6f" hs_bindgen_test_names_3513093206483c6f
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE capi_ptr #-}

capi_ptr :: Ptr.FunPtr (IO ())
capi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_3513093206483c6f

{-| __C declaration:__ @cases@

    __defined at:__ @names.h:14:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_af7d0bdd6562f7d9" hs_bindgen_test_names_af7d0bdd6562f7d9
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cases_ptr #-}

cases_ptr :: Ptr.FunPtr (IO ())
cases_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_af7d0bdd6562f7d9

{-| __C declaration:__ @ccall@

    __defined at:__ @names.h:15:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_79b6583362113331" hs_bindgen_test_names_79b6583362113331
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE ccall_ptr #-}

ccall_ptr :: Ptr.FunPtr (IO ())
ccall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_79b6583362113331

{-| __C declaration:__ @dynamic@

    __defined at:__ @names.h:16:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_5c19071f2f4eaf45" hs_bindgen_test_names_5c19071f2f4eaf45
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE dynamic_ptr #-}

dynamic_ptr :: Ptr.FunPtr (IO ())
dynamic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_5c19071f2f4eaf45

{-| __C declaration:__ @export@

    __defined at:__ @names.h:17:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_98505ac7a8664e29" hs_bindgen_test_names_98505ac7a8664e29
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE export_ptr #-}

export_ptr :: Ptr.FunPtr (IO ())
export_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_98505ac7a8664e29

{-| __C declaration:__ @family@

    __defined at:__ @names.h:18:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_849e77496a12f443" hs_bindgen_test_names_849e77496a12f443
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE family_ptr #-}

family_ptr :: Ptr.FunPtr (IO ())
family_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_849e77496a12f443

{-| __C declaration:__ @group@

    __defined at:__ @names.h:19:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_41e72434cbcfa0d5" hs_bindgen_test_names_41e72434cbcfa0d5
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE group_ptr #-}

group_ptr :: Ptr.FunPtr (IO ())
group_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_41e72434cbcfa0d5

{-| __C declaration:__ @interruptible@

    __defined at:__ @names.h:20:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_d016728709223884" hs_bindgen_test_names_d016728709223884
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE interruptible_ptr #-}

interruptible_ptr :: Ptr.FunPtr (IO ())
interruptible_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_d016728709223884

{-| __C declaration:__ @javascript@

    __defined at:__ @names.h:21:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_fa5819996081f2b8" hs_bindgen_test_names_fa5819996081f2b8
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE javascript_ptr #-}

javascript_ptr :: Ptr.FunPtr (IO ())
javascript_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_fa5819996081f2b8

{-| __C declaration:__ @label@

    __defined at:__ @names.h:22:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_0004d966bf5f8027" hs_bindgen_test_names_0004d966bf5f8027
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE label_ptr #-}

label_ptr :: Ptr.FunPtr (IO ())
label_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_0004d966bf5f8027

{-| __C declaration:__ @prim@

    __defined at:__ @names.h:23:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_d9ec78c543d7f0ea" hs_bindgen_test_names_d9ec78c543d7f0ea
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE prim_ptr #-}

prim_ptr :: Ptr.FunPtr (IO ())
prim_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_d9ec78c543d7f0ea

{-| __C declaration:__ @role@

    __defined at:__ @names.h:24:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_89449e7f8f90fc97" hs_bindgen_test_names_89449e7f8f90fc97
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE role_ptr #-}

role_ptr :: Ptr.FunPtr (IO ())
role_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_89449e7f8f90fc97

{-| __C declaration:__ @safe@

    __defined at:__ @names.h:25:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_983c057a9645fd29" hs_bindgen_test_names_983c057a9645fd29
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE safe_ptr #-}

safe_ptr :: Ptr.FunPtr (IO ())
safe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_983c057a9645fd29

{-| __C declaration:__ @stdcall@

    __defined at:__ @names.h:26:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_8ea4a9581868a08c" hs_bindgen_test_names_8ea4a9581868a08c
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stdcall_ptr #-}

stdcall_ptr :: Ptr.FunPtr (IO ())
stdcall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_8ea4a9581868a08c

{-| __C declaration:__ @stock@

    __defined at:__ @names.h:27:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_9d4197174428177d" hs_bindgen_test_names_9d4197174428177d
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stock_ptr #-}

stock_ptr :: Ptr.FunPtr (IO ())
stock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_9d4197174428177d

{-| __C declaration:__ @unsafe@

    __defined at:__ @names.h:28:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_6eb48adc74567d6d" hs_bindgen_test_names_6eb48adc74567d6d
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE unsafe_ptr #-}

unsafe_ptr :: Ptr.FunPtr (IO ())
unsafe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_6eb48adc74567d6d

{-| __C declaration:__ @via@

    __defined at:__ @names.h:29:6@

    __exported by:__ @names.h@
-}
foreign import ccall unsafe "hs_bindgen_test_names_64eddb721da8c268" hs_bindgen_test_names_64eddb721da8c268
  :: IO (Ptr.FunPtr (IO ()))

{-# NOINLINE via_ptr #-}

via_ptr :: Ptr.FunPtr (IO ())
via_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_names_64eddb721da8c268
