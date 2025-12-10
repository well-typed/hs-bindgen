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
  , "/* test_edgecasesnames_Example_get_by_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6974dc9d1fd9efdb (void)) (void)"
  , "{"
  , "  return &by;"
  , "}"
  , "/* test_edgecasesnames_Example_get_forall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3b643ea94c4ffa20 (void)) (void)"
  , "{"
  , "  return &forall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_mdo_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_75a958dcb6aa760d (void)) (void)"
  , "{"
  , "  return &mdo;"
  , "}"
  , "/* test_edgecasesnames_Example_get_pattern_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2dca22d927c6b6c9 (void)) (void)"
  , "{"
  , "  return &pattern;"
  , "}"
  , "/* test_edgecasesnames_Example_get_proc_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_278f3b1df8a83886 (void)) (void)"
  , "{"
  , "  return &proc;"
  , "}"
  , "/* test_edgecasesnames_Example_get_rec_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3be0a960ee51c3e9 (void)) (void)"
  , "{"
  , "  return &rec;"
  , "}"
  , "/* test_edgecasesnames_Example_get_using_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_84e44d3bf799af26 (void)) (void)"
  , "{"
  , "  return &using;"
  , "}"
  , "/* test_edgecasesnames_Example_get_anyclass_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_95cf172ae2160046 (void)) (void)"
  , "{"
  , "  return &anyclass;"
  , "}"
  , "/* test_edgecasesnames_Example_get_capi_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ea4d99f6c2e96742 (void)) (void)"
  , "{"
  , "  return &capi;"
  , "}"
  , "/* test_edgecasesnames_Example_get_cases_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aff69f10c4c30a0d (void)) (void)"
  , "{"
  , "  return &cases;"
  , "}"
  , "/* test_edgecasesnames_Example_get_ccall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f5db0ca2d6dce4d5 (void)) (void)"
  , "{"
  , "  return &ccall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_dynamic_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d5e3711b7b2f435e (void)) (void)"
  , "{"
  , "  return &dynamic;"
  , "}"
  , "/* test_edgecasesnames_Example_get_export_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c3e4c5611dd8ffdf (void)) (void)"
  , "{"
  , "  return &export;"
  , "}"
  , "/* test_edgecasesnames_Example_get_family_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_658a25f6c844805b (void)) (void)"
  , "{"
  , "  return &family;"
  , "}"
  , "/* test_edgecasesnames_Example_get_group_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0c7fb62fb95f0f38 (void)) (void)"
  , "{"
  , "  return &group;"
  , "}"
  , "/* test_edgecasesnames_Example_get_interruptible_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bb9472bbc501c78f (void)) (void)"
  , "{"
  , "  return &interruptible;"
  , "}"
  , "/* test_edgecasesnames_Example_get_javascript_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0c7eeee673af7865 (void)) (void)"
  , "{"
  , "  return &javascript;"
  , "}"
  , "/* test_edgecasesnames_Example_get_label_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d16291c6c6c905ab (void)) (void)"
  , "{"
  , "  return &label;"
  , "}"
  , "/* test_edgecasesnames_Example_get_prim_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0657843e52c044fe (void)) (void)"
  , "{"
  , "  return &prim;"
  , "}"
  , "/* test_edgecasesnames_Example_get_role_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bdbcb244d39fa251 (void)) (void)"
  , "{"
  , "  return &role;"
  , "}"
  , "/* test_edgecasesnames_Example_get_safe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_400c28e571f13194 (void)) (void)"
  , "{"
  , "  return &safe;"
  , "}"
  , "/* test_edgecasesnames_Example_get_stdcall_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_15aff4a3542e1023 (void)) (void)"
  , "{"
  , "  return &stdcall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_stock_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9df7dc3f71a3ab76 (void)) (void)"
  , "{"
  , "  return &stock;"
  , "}"
  , "/* test_edgecasesnames_Example_get_unsafe_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aebb5b55a2d78a79 (void)) (void)"
  , "{"
  , "  return &unsafe;"
  , "}"
  , "/* test_edgecasesnames_Example_get_via_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c6fe1f3a125fa32d (void)) (void)"
  , "{"
  , "  return &via;"
  , "}"
  ]))

-- | __unique:__ @test_edgecasesnames_Example_get_by_ptr@
foreign import ccall unsafe "hs_bindgen_6974dc9d1fd9efdb" hs_bindgen_6974dc9d1fd9efdb ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE by'_ptr #-}

{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h:3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by'_ptr :: Ptr.FunPtr (IO ())
by'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6974dc9d1fd9efdb

-- | __unique:__ @test_edgecasesnames_Example_get_forall_ptr@
foreign import ccall unsafe "hs_bindgen_3b643ea94c4ffa20" hs_bindgen_3b643ea94c4ffa20 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE forall'_ptr #-}

{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h:4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall'_ptr :: Ptr.FunPtr (IO ())
forall'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3b643ea94c4ffa20

-- | __unique:__ @test_edgecasesnames_Example_get_mdo_ptr@
foreign import ccall unsafe "hs_bindgen_75a958dcb6aa760d" hs_bindgen_75a958dcb6aa760d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE mdo'_ptr #-}

{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h:5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo'_ptr :: Ptr.FunPtr (IO ())
mdo'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_75a958dcb6aa760d

-- | __unique:__ @test_edgecasesnames_Example_get_pattern_ptr@
foreign import ccall unsafe "hs_bindgen_2dca22d927c6b6c9" hs_bindgen_2dca22d927c6b6c9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE pattern'_ptr #-}

{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h:6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern'_ptr :: Ptr.FunPtr (IO ())
pattern'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2dca22d927c6b6c9

-- | __unique:__ @test_edgecasesnames_Example_get_proc_ptr@
foreign import ccall unsafe "hs_bindgen_278f3b1df8a83886" hs_bindgen_278f3b1df8a83886 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE proc'_ptr #-}

{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h:7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc'_ptr :: Ptr.FunPtr (IO ())
proc'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_278f3b1df8a83886

-- | __unique:__ @test_edgecasesnames_Example_get_rec_ptr@
foreign import ccall unsafe "hs_bindgen_3be0a960ee51c3e9" hs_bindgen_3be0a960ee51c3e9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE rec'_ptr #-}

{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h:8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec'_ptr :: Ptr.FunPtr (IO ())
rec'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3be0a960ee51c3e9

-- | __unique:__ @test_edgecasesnames_Example_get_using_ptr@
foreign import ccall unsafe "hs_bindgen_84e44d3bf799af26" hs_bindgen_84e44d3bf799af26 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE using'_ptr #-}

{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h:9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using'_ptr :: Ptr.FunPtr (IO ())
using'_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_84e44d3bf799af26

-- | __unique:__ @test_edgecasesnames_Example_get_anyclass_ptr@
foreign import ccall unsafe "hs_bindgen_95cf172ae2160046" hs_bindgen_95cf172ae2160046 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE anyclass_ptr #-}

{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h:12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass_ptr :: Ptr.FunPtr (IO ())
anyclass_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_95cf172ae2160046

-- | __unique:__ @test_edgecasesnames_Example_get_capi_ptr@
foreign import ccall unsafe "hs_bindgen_ea4d99f6c2e96742" hs_bindgen_ea4d99f6c2e96742 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE capi_ptr #-}

{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h:13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi_ptr :: Ptr.FunPtr (IO ())
capi_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ea4d99f6c2e96742

-- | __unique:__ @test_edgecasesnames_Example_get_cases_ptr@
foreign import ccall unsafe "hs_bindgen_aff69f10c4c30a0d" hs_bindgen_aff69f10c4c30a0d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE cases_ptr #-}

{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h:14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases_ptr :: Ptr.FunPtr (IO ())
cases_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aff69f10c4c30a0d

-- | __unique:__ @test_edgecasesnames_Example_get_ccall_ptr@
foreign import ccall unsafe "hs_bindgen_f5db0ca2d6dce4d5" hs_bindgen_f5db0ca2d6dce4d5 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE ccall_ptr #-}

{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h:15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall_ptr :: Ptr.FunPtr (IO ())
ccall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f5db0ca2d6dce4d5

-- | __unique:__ @test_edgecasesnames_Example_get_dynamic_ptr@
foreign import ccall unsafe "hs_bindgen_d5e3711b7b2f435e" hs_bindgen_d5e3711b7b2f435e ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE dynamic_ptr #-}

{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h:16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic_ptr :: Ptr.FunPtr (IO ())
dynamic_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5e3711b7b2f435e

-- | __unique:__ @test_edgecasesnames_Example_get_export_ptr@
foreign import ccall unsafe "hs_bindgen_c3e4c5611dd8ffdf" hs_bindgen_c3e4c5611dd8ffdf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE export_ptr #-}

{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h:17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export_ptr :: Ptr.FunPtr (IO ())
export_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c3e4c5611dd8ffdf

-- | __unique:__ @test_edgecasesnames_Example_get_family_ptr@
foreign import ccall unsafe "hs_bindgen_658a25f6c844805b" hs_bindgen_658a25f6c844805b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE family_ptr #-}

{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h:18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family_ptr :: Ptr.FunPtr (IO ())
family_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_658a25f6c844805b

-- | __unique:__ @test_edgecasesnames_Example_get_group_ptr@
foreign import ccall unsafe "hs_bindgen_0c7fb62fb95f0f38" hs_bindgen_0c7fb62fb95f0f38 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE group_ptr #-}

{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h:19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group_ptr :: Ptr.FunPtr (IO ())
group_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0c7fb62fb95f0f38

-- | __unique:__ @test_edgecasesnames_Example_get_interruptible_ptr@
foreign import ccall unsafe "hs_bindgen_bb9472bbc501c78f" hs_bindgen_bb9472bbc501c78f ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE interruptible_ptr #-}

{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h:20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible_ptr :: Ptr.FunPtr (IO ())
interruptible_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb9472bbc501c78f

-- | __unique:__ @test_edgecasesnames_Example_get_javascript_ptr@
foreign import ccall unsafe "hs_bindgen_0c7eeee673af7865" hs_bindgen_0c7eeee673af7865 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE javascript_ptr #-}

{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h:21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript_ptr :: Ptr.FunPtr (IO ())
javascript_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0c7eeee673af7865

-- | __unique:__ @test_edgecasesnames_Example_get_label_ptr@
foreign import ccall unsafe "hs_bindgen_d16291c6c6c905ab" hs_bindgen_d16291c6c6c905ab ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE label_ptr #-}

{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h:22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label_ptr :: Ptr.FunPtr (IO ())
label_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d16291c6c6c905ab

-- | __unique:__ @test_edgecasesnames_Example_get_prim_ptr@
foreign import ccall unsafe "hs_bindgen_0657843e52c044fe" hs_bindgen_0657843e52c044fe ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE prim_ptr #-}

{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h:23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim_ptr :: Ptr.FunPtr (IO ())
prim_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0657843e52c044fe

-- | __unique:__ @test_edgecasesnames_Example_get_role_ptr@
foreign import ccall unsafe "hs_bindgen_bdbcb244d39fa251" hs_bindgen_bdbcb244d39fa251 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE role_ptr #-}

{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h:24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role_ptr :: Ptr.FunPtr (IO ())
role_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bdbcb244d39fa251

-- | __unique:__ @test_edgecasesnames_Example_get_safe_ptr@
foreign import ccall unsafe "hs_bindgen_400c28e571f13194" hs_bindgen_400c28e571f13194 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE safe_ptr #-}

{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h:25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe_ptr :: Ptr.FunPtr (IO ())
safe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_400c28e571f13194

-- | __unique:__ @test_edgecasesnames_Example_get_stdcall_ptr@
foreign import ccall unsafe "hs_bindgen_15aff4a3542e1023" hs_bindgen_15aff4a3542e1023 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stdcall_ptr #-}

{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h:26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall_ptr :: Ptr.FunPtr (IO ())
stdcall_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_15aff4a3542e1023

-- | __unique:__ @test_edgecasesnames_Example_get_stock_ptr@
foreign import ccall unsafe "hs_bindgen_9df7dc3f71a3ab76" hs_bindgen_9df7dc3f71a3ab76 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE stock_ptr #-}

{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h:27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock_ptr :: Ptr.FunPtr (IO ())
stock_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9df7dc3f71a3ab76

-- | __unique:__ @test_edgecasesnames_Example_get_unsafe_ptr@
foreign import ccall unsafe "hs_bindgen_aebb5b55a2d78a79" hs_bindgen_aebb5b55a2d78a79 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE unsafe_ptr #-}

{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h:28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe_ptr :: Ptr.FunPtr (IO ())
unsafe_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aebb5b55a2d78a79

-- | __unique:__ @test_edgecasesnames_Example_get_via_ptr@
foreign import ccall unsafe "hs_bindgen_c6fe1f3a125fa32d" hs_bindgen_c6fe1f3a125fa32d ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE via_ptr #-}

{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h:29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via_ptr :: Ptr.FunPtr (IO ())
via_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c6fe1f3a125fa32d
