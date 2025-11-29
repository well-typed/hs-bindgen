{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/names.h>"
  , "/* Example_get_by_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_4721f2de56bee5a7 (void)) (void)"
  , "{"
  , "  return &by;"
  , "}"
  , "/* Example_get_forall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_a6c5e2959eea689d (void)) (void)"
  , "{"
  , "  return &forall;"
  , "}"
  , "/* Example_get_mdo_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_c8170d8a02bd9d63 (void)) (void)"
  , "{"
  , "  return &mdo;"
  , "}"
  , "/* Example_get_pattern_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_ecf0921b33f20e24 (void)) (void)"
  , "{"
  , "  return &pattern;"
  , "}"
  , "/* Example_get_proc_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_7b723fb1b3bb127b (void)) (void)"
  , "{"
  , "  return &proc;"
  , "}"
  , "/* Example_get_rec_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_2141b591f98fe9a9 (void)) (void)"
  , "{"
  , "  return &rec;"
  , "}"
  , "/* Example_get_using_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_fe29e67ea061b330 (void)) (void)"
  , "{"
  , "  return &using;"
  , "}"
  , "/* Example_get_anyclass_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_cfa29e557736efd5 (void)) (void)"
  , "{"
  , "  return &anyclass;"
  , "}"
  , "/* Example_get_capi_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_ae41a085ef9adcef (void)) (void)"
  , "{"
  , "  return &capi;"
  , "}"
  , "/* Example_get_cases_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_cc15e121c247d854 (void)) (void)"
  , "{"
  , "  return &cases;"
  , "}"
  , "/* Example_get_ccall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_2f1be0c0b0549873 (void)) (void)"
  , "{"
  , "  return &ccall;"
  , "}"
  , "/* Example_get_dynamic_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_7dea2fc6915321c7 (void)) (void)"
  , "{"
  , "  return &dynamic;"
  , "}"
  , "/* Example_get_export_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_987942fc1f4b3233 (void)) (void)"
  , "{"
  , "  return &export;"
  , "}"
  , "/* Example_get_family_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_5c138e5e14a493dd (void)) (void)"
  , "{"
  , "  return &family;"
  , "}"
  , "/* Example_get_group_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_5cb9b135d109a0be (void)) (void)"
  , "{"
  , "  return &group;"
  , "}"
  , "/* Example_get_interruptible_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_2debad19a93cb81a (void)) (void)"
  , "{"
  , "  return &interruptible;"
  , "}"
  , "/* Example_get_javascript_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_361dc2d1e5f57ae1 (void)) (void)"
  , "{"
  , "  return &javascript;"
  , "}"
  , "/* Example_get_label_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_20beffa5b46156da (void)) (void)"
  , "{"
  , "  return &label;"
  , "}"
  , "/* Example_get_prim_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_eefe3fdd7675b8f9 (void)) (void)"
  , "{"
  , "  return &prim;"
  , "}"
  , "/* Example_get_role_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_40365f1ff4f34eee (void)) (void)"
  , "{"
  , "  return &role;"
  , "}"
  , "/* Example_get_safe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_2ef470906d3c3bef (void)) (void)"
  , "{"
  , "  return &safe;"
  , "}"
  , "/* Example_get_stdcall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_f4fc85943aaf14e2 (void)) (void)"
  , "{"
  , "  return &stdcall;"
  , "}"
  , "/* Example_get_stock_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_b762fa5d4a81e02a (void)) (void)"
  , "{"
  , "  return &stock;"
  , "}"
  , "/* Example_get_unsafe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_b11ceb7c40dc58b3 (void)) (void)"
  , "{"
  , "  return &unsafe;"
  , "}"
  , "/* Example_get_via_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_edgecasesnames_4e4ca9e7e1239e1e (void)) (void)"
  , "{"
  , "  return &via;"
  , "}"
  ]))

{-| __unique:__ @Example_get_by_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_4721f2de56bee5a7" hs_bindgen_test_edgecasesnames_4721f2de56bee5a7 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE by'_ptr #-}

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by'_ptr :: Ptr.FunPtr (IO ())
by'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_4721f2de56bee5a7

{-| __unique:__ @Example_get_forall_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_a6c5e2959eea689d" hs_bindgen_test_edgecasesnames_a6c5e2959eea689d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE forall'_ptr #-}

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall'_ptr :: Ptr.FunPtr (IO ())
forall'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_a6c5e2959eea689d

{-| __unique:__ @Example_get_mdo_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_c8170d8a02bd9d63" hs_bindgen_test_edgecasesnames_c8170d8a02bd9d63 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE mdo'_ptr #-}

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo'_ptr :: Ptr.FunPtr (IO ())
mdo'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_c8170d8a02bd9d63

{-| __unique:__ @Example_get_pattern_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ecf0921b33f20e24" hs_bindgen_test_edgecasesnames_ecf0921b33f20e24 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE pattern'_ptr #-}

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern'_ptr :: Ptr.FunPtr (IO ())
pattern'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_ecf0921b33f20e24

{-| __unique:__ @Example_get_proc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_7b723fb1b3bb127b" hs_bindgen_test_edgecasesnames_7b723fb1b3bb127b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE proc'_ptr #-}

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc'_ptr :: Ptr.FunPtr (IO ())
proc'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_7b723fb1b3bb127b

{-| __unique:__ @Example_get_rec_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2141b591f98fe9a9" hs_bindgen_test_edgecasesnames_2141b591f98fe9a9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE rec'_ptr #-}

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec'_ptr :: Ptr.FunPtr (IO ())
rec'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_2141b591f98fe9a9

{-| __unique:__ @Example_get_using_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_fe29e67ea061b330" hs_bindgen_test_edgecasesnames_fe29e67ea061b330 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE using'_ptr #-}

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using'_ptr :: Ptr.FunPtr (IO ())
using'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_fe29e67ea061b330

{-| __unique:__ @Example_get_anyclass_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_cfa29e557736efd5" hs_bindgen_test_edgecasesnames_cfa29e557736efd5 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE anyclass_ptr #-}

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass_ptr :: Ptr.FunPtr (IO ())
anyclass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_cfa29e557736efd5

{-| __unique:__ @Example_get_capi_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_ae41a085ef9adcef" hs_bindgen_test_edgecasesnames_ae41a085ef9adcef ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE capi_ptr #-}

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi_ptr :: Ptr.FunPtr (IO ())
capi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_ae41a085ef9adcef

{-| __unique:__ @Example_get_cases_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_cc15e121c247d854" hs_bindgen_test_edgecasesnames_cc15e121c247d854 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cases_ptr #-}

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases_ptr :: Ptr.FunPtr (IO ())
cases_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_cc15e121c247d854

{-| __unique:__ @Example_get_ccall_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2f1be0c0b0549873" hs_bindgen_test_edgecasesnames_2f1be0c0b0549873 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE ccall_ptr #-}

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall_ptr :: Ptr.FunPtr (IO ())
ccall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_2f1be0c0b0549873

{-| __unique:__ @Example_get_dynamic_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_7dea2fc6915321c7" hs_bindgen_test_edgecasesnames_7dea2fc6915321c7 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE dynamic_ptr #-}

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic_ptr :: Ptr.FunPtr (IO ())
dynamic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_7dea2fc6915321c7

{-| __unique:__ @Example_get_export_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_987942fc1f4b3233" hs_bindgen_test_edgecasesnames_987942fc1f4b3233 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE export_ptr #-}

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export_ptr :: Ptr.FunPtr (IO ())
export_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_987942fc1f4b3233

{-| __unique:__ @Example_get_family_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_5c138e5e14a493dd" hs_bindgen_test_edgecasesnames_5c138e5e14a493dd ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE family_ptr #-}

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family_ptr :: Ptr.FunPtr (IO ())
family_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_5c138e5e14a493dd

{-| __unique:__ @Example_get_group_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_5cb9b135d109a0be" hs_bindgen_test_edgecasesnames_5cb9b135d109a0be ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE group_ptr #-}

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group_ptr :: Ptr.FunPtr (IO ())
group_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_5cb9b135d109a0be

{-| __unique:__ @Example_get_interruptible_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2debad19a93cb81a" hs_bindgen_test_edgecasesnames_2debad19a93cb81a ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE interruptible_ptr #-}

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible_ptr :: Ptr.FunPtr (IO ())
interruptible_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_2debad19a93cb81a

{-| __unique:__ @Example_get_javascript_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_361dc2d1e5f57ae1" hs_bindgen_test_edgecasesnames_361dc2d1e5f57ae1 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE javascript_ptr #-}

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript_ptr :: Ptr.FunPtr (IO ())
javascript_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_361dc2d1e5f57ae1

{-| __unique:__ @Example_get_label_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_20beffa5b46156da" hs_bindgen_test_edgecasesnames_20beffa5b46156da ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE label_ptr #-}

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label_ptr :: Ptr.FunPtr (IO ())
label_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_20beffa5b46156da

{-| __unique:__ @Example_get_prim_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_eefe3fdd7675b8f9" hs_bindgen_test_edgecasesnames_eefe3fdd7675b8f9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE prim_ptr #-}

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim_ptr :: Ptr.FunPtr (IO ())
prim_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_eefe3fdd7675b8f9

{-| __unique:__ @Example_get_role_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_40365f1ff4f34eee" hs_bindgen_test_edgecasesnames_40365f1ff4f34eee ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE role_ptr #-}

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role_ptr :: Ptr.FunPtr (IO ())
role_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_40365f1ff4f34eee

{-| __unique:__ @Example_get_safe_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_2ef470906d3c3bef" hs_bindgen_test_edgecasesnames_2ef470906d3c3bef ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE safe_ptr #-}

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe_ptr :: Ptr.FunPtr (IO ())
safe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_2ef470906d3c3bef

{-| __unique:__ @Example_get_stdcall_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_f4fc85943aaf14e2" hs_bindgen_test_edgecasesnames_f4fc85943aaf14e2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stdcall_ptr #-}

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall_ptr :: Ptr.FunPtr (IO ())
stdcall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_f4fc85943aaf14e2

{-| __unique:__ @Example_get_stock_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_b762fa5d4a81e02a" hs_bindgen_test_edgecasesnames_b762fa5d4a81e02a ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stock_ptr #-}

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock_ptr :: Ptr.FunPtr (IO ())
stock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_b762fa5d4a81e02a

{-| __unique:__ @Example_get_unsafe_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_b11ceb7c40dc58b3" hs_bindgen_test_edgecasesnames_b11ceb7c40dc58b3 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE unsafe_ptr #-}

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe_ptr :: Ptr.FunPtr (IO ())
unsafe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_b11ceb7c40dc58b3

{-| __unique:__ @Example_get_via_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesnames_4e4ca9e7e1239e1e" hs_bindgen_test_edgecasesnames_4e4ca9e7e1239e1e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE via_ptr #-}

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via_ptr :: Ptr.FunPtr (IO ())
via_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_edgecasesnames_4e4ca9e7e1239e1e
