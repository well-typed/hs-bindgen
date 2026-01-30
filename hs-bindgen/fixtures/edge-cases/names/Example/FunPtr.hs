{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <edge-cases/names.h>"
  , "/* test_edgecasesnames_Example_get_by */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d50b20f504174dd5 (void)) (void)"
  , "{"
  , "  return &by;"
  , "}"
  , "/* test_edgecasesnames_Example_get_forall */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f7e660ea9223a909 (void)) (void)"
  , "{"
  , "  return &forall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_mdo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_15319d1b8d2ca927 (void)) (void)"
  , "{"
  , "  return &mdo;"
  , "}"
  , "/* test_edgecasesnames_Example_get_pattern */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e5a1961c11d641ed (void)) (void)"
  , "{"
  , "  return &pattern;"
  , "}"
  , "/* test_edgecasesnames_Example_get_proc */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d2dd74adbb4a606c (void)) (void)"
  , "{"
  , "  return &proc;"
  , "}"
  , "/* test_edgecasesnames_Example_get_rec */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bab6d9c9540c9d58 (void)) (void)"
  , "{"
  , "  return &rec;"
  , "}"
  , "/* test_edgecasesnames_Example_get_using */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_447fa7223c86f5c7 (void)) (void)"
  , "{"
  , "  return &using;"
  , "}"
  , "/* test_edgecasesnames_Example_get_anyclass */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f2292c17facdbcde (void)) (void)"
  , "{"
  , "  return &anyclass;"
  , "}"
  , "/* test_edgecasesnames_Example_get_capi */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_393b62c1a821de61 (void)) (void)"
  , "{"
  , "  return &capi;"
  , "}"
  , "/* test_edgecasesnames_Example_get_cases */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_962b5bf9813ea6dd (void)) (void)"
  , "{"
  , "  return &cases;"
  , "}"
  , "/* test_edgecasesnames_Example_get_ccall */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_599948844183b4ea (void)) (void)"
  , "{"
  , "  return &ccall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_dynamic */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_aa442ea764d9d93c (void)) (void)"
  , "{"
  , "  return &dynamic;"
  , "}"
  , "/* test_edgecasesnames_Example_get_export */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_98dfb21e9f7b6857 (void)) (void)"
  , "{"
  , "  return &export;"
  , "}"
  , "/* test_edgecasesnames_Example_get_family */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b96b9fd3fa689c75 (void)) (void)"
  , "{"
  , "  return &family;"
  , "}"
  , "/* test_edgecasesnames_Example_get_group */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a4413aed06e03d16 (void)) (void)"
  , "{"
  , "  return &group;"
  , "}"
  , "/* test_edgecasesnames_Example_get_interruptible */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_52223789a2752bbe (void)) (void)"
  , "{"
  , "  return &interruptible;"
  , "}"
  , "/* test_edgecasesnames_Example_get_javascript */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3d349ab20e55d3d5 (void)) (void)"
  , "{"
  , "  return &javascript;"
  , "}"
  , "/* test_edgecasesnames_Example_get_label */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7ab78b24e63738f5 (void)) (void)"
  , "{"
  , "  return &label;"
  , "}"
  , "/* test_edgecasesnames_Example_get_prim */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_13caa66b81eebdfb (void)) (void)"
  , "{"
  , "  return &prim;"
  , "}"
  , "/* test_edgecasesnames_Example_get_role */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_99189a38ba22fdad (void)) (void)"
  , "{"
  , "  return &role;"
  , "}"
  , "/* test_edgecasesnames_Example_get_safe */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fe3819b9444e41b6 (void)) (void)"
  , "{"
  , "  return &safe;"
  , "}"
  , "/* test_edgecasesnames_Example_get_stdcall */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7d4e52bd26694f13 (void)) (void)"
  , "{"
  , "  return &stdcall;"
  , "}"
  , "/* test_edgecasesnames_Example_get_stock */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_be2fd8368b1f85bc (void)) (void)"
  , "{"
  , "  return &stock;"
  , "}"
  , "/* test_edgecasesnames_Example_get_unsafe */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d0faf7702a235ae1 (void)) (void)"
  , "{"
  , "  return &unsafe;"
  , "}"
  , "/* test_edgecasesnames_Example_get_via */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_56fc690ff4bc7c4e (void)) (void)"
  , "{"
  , "  return &via;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesnames_Example_get_by@
foreign import ccall unsafe "hs_bindgen_d50b20f504174dd5" hs_bindgen_d50b20f504174dd5_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_by@
hs_bindgen_d50b20f504174dd5 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_d50b20f504174dd5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d50b20f504174dd5_base

{-# NOINLINE by'_funptr #-}
{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h 3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by'_funptr :: Ptr.FunPtr (IO ())
by'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d50b20f504174dd5

-- __unique:__ @test_edgecasesnames_Example_get_forall@
foreign import ccall unsafe "hs_bindgen_f7e660ea9223a909" hs_bindgen_f7e660ea9223a909_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_forall@
hs_bindgen_f7e660ea9223a909 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_f7e660ea9223a909 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f7e660ea9223a909_base

{-# NOINLINE forall'_funptr #-}
{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h 4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall'_funptr :: Ptr.FunPtr (IO ())
forall'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f7e660ea9223a909

-- __unique:__ @test_edgecasesnames_Example_get_mdo@
foreign import ccall unsafe "hs_bindgen_15319d1b8d2ca927" hs_bindgen_15319d1b8d2ca927_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_mdo@
hs_bindgen_15319d1b8d2ca927 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_15319d1b8d2ca927 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_15319d1b8d2ca927_base

{-# NOINLINE mdo'_funptr #-}
{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h 5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo'_funptr :: Ptr.FunPtr (IO ())
mdo'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_15319d1b8d2ca927

-- __unique:__ @test_edgecasesnames_Example_get_pattern@
foreign import ccall unsafe "hs_bindgen_e5a1961c11d641ed" hs_bindgen_e5a1961c11d641ed_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_pattern@
hs_bindgen_e5a1961c11d641ed :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_e5a1961c11d641ed =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e5a1961c11d641ed_base

{-# NOINLINE pattern'_funptr #-}
{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h 6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern'_funptr :: Ptr.FunPtr (IO ())
pattern'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e5a1961c11d641ed

-- __unique:__ @test_edgecasesnames_Example_get_proc@
foreign import ccall unsafe "hs_bindgen_d2dd74adbb4a606c" hs_bindgen_d2dd74adbb4a606c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_proc@
hs_bindgen_d2dd74adbb4a606c :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_d2dd74adbb4a606c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d2dd74adbb4a606c_base

{-# NOINLINE proc'_funptr #-}
{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h 7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc'_funptr :: Ptr.FunPtr (IO ())
proc'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d2dd74adbb4a606c

-- __unique:__ @test_edgecasesnames_Example_get_rec@
foreign import ccall unsafe "hs_bindgen_bab6d9c9540c9d58" hs_bindgen_bab6d9c9540c9d58_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_rec@
hs_bindgen_bab6d9c9540c9d58 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_bab6d9c9540c9d58 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_bab6d9c9540c9d58_base

{-# NOINLINE rec'_funptr #-}
{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h 8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec'_funptr :: Ptr.FunPtr (IO ())
rec'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bab6d9c9540c9d58

-- __unique:__ @test_edgecasesnames_Example_get_using@
foreign import ccall unsafe "hs_bindgen_447fa7223c86f5c7" hs_bindgen_447fa7223c86f5c7_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_using@
hs_bindgen_447fa7223c86f5c7 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_447fa7223c86f5c7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_447fa7223c86f5c7_base

{-# NOINLINE using'_funptr #-}
{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h 9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using'_funptr :: Ptr.FunPtr (IO ())
using'_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_447fa7223c86f5c7

-- __unique:__ @test_edgecasesnames_Example_get_anyclass@
foreign import ccall unsafe "hs_bindgen_f2292c17facdbcde" hs_bindgen_f2292c17facdbcde_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_anyclass@
hs_bindgen_f2292c17facdbcde :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_f2292c17facdbcde =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f2292c17facdbcde_base

{-# NOINLINE anyclass_funptr #-}
{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h 12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass_funptr :: Ptr.FunPtr (IO ())
anyclass_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f2292c17facdbcde

-- __unique:__ @test_edgecasesnames_Example_get_capi@
foreign import ccall unsafe "hs_bindgen_393b62c1a821de61" hs_bindgen_393b62c1a821de61_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_capi@
hs_bindgen_393b62c1a821de61 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_393b62c1a821de61 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_393b62c1a821de61_base

{-# NOINLINE capi_funptr #-}
{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h 13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi_funptr :: Ptr.FunPtr (IO ())
capi_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_393b62c1a821de61

-- __unique:__ @test_edgecasesnames_Example_get_cases@
foreign import ccall unsafe "hs_bindgen_962b5bf9813ea6dd" hs_bindgen_962b5bf9813ea6dd_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_cases@
hs_bindgen_962b5bf9813ea6dd :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_962b5bf9813ea6dd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_962b5bf9813ea6dd_base

{-# NOINLINE cases_funptr #-}
{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h 14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases_funptr :: Ptr.FunPtr (IO ())
cases_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_962b5bf9813ea6dd

-- __unique:__ @test_edgecasesnames_Example_get_ccall@
foreign import ccall unsafe "hs_bindgen_599948844183b4ea" hs_bindgen_599948844183b4ea_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_ccall@
hs_bindgen_599948844183b4ea :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_599948844183b4ea =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_599948844183b4ea_base

{-# NOINLINE ccall_funptr #-}
{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h 15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall_funptr :: Ptr.FunPtr (IO ())
ccall_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_599948844183b4ea

-- __unique:__ @test_edgecasesnames_Example_get_dynamic@
foreign import ccall unsafe "hs_bindgen_aa442ea764d9d93c" hs_bindgen_aa442ea764d9d93c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_dynamic@
hs_bindgen_aa442ea764d9d93c :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_aa442ea764d9d93c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_aa442ea764d9d93c_base

{-# NOINLINE dynamic_funptr #-}
{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h 16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic_funptr :: Ptr.FunPtr (IO ())
dynamic_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa442ea764d9d93c

-- __unique:__ @test_edgecasesnames_Example_get_export@
foreign import ccall unsafe "hs_bindgen_98dfb21e9f7b6857" hs_bindgen_98dfb21e9f7b6857_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_export@
hs_bindgen_98dfb21e9f7b6857 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_98dfb21e9f7b6857 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_98dfb21e9f7b6857_base

{-# NOINLINE export_funptr #-}
{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h 17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export_funptr :: Ptr.FunPtr (IO ())
export_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_98dfb21e9f7b6857

-- __unique:__ @test_edgecasesnames_Example_get_family@
foreign import ccall unsafe "hs_bindgen_b96b9fd3fa689c75" hs_bindgen_b96b9fd3fa689c75_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_family@
hs_bindgen_b96b9fd3fa689c75 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_b96b9fd3fa689c75 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b96b9fd3fa689c75_base

{-# NOINLINE family_funptr #-}
{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h 18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family_funptr :: Ptr.FunPtr (IO ())
family_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b96b9fd3fa689c75

-- __unique:__ @test_edgecasesnames_Example_get_group@
foreign import ccall unsafe "hs_bindgen_a4413aed06e03d16" hs_bindgen_a4413aed06e03d16_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_group@
hs_bindgen_a4413aed06e03d16 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_a4413aed06e03d16 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a4413aed06e03d16_base

{-# NOINLINE group_funptr #-}
{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h 19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group_funptr :: Ptr.FunPtr (IO ())
group_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a4413aed06e03d16

-- __unique:__ @test_edgecasesnames_Example_get_interruptible@
foreign import ccall unsafe "hs_bindgen_52223789a2752bbe" hs_bindgen_52223789a2752bbe_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_interruptible@
hs_bindgen_52223789a2752bbe :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_52223789a2752bbe =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_52223789a2752bbe_base

{-# NOINLINE interruptible_funptr #-}
{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h 20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible_funptr :: Ptr.FunPtr (IO ())
interruptible_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_52223789a2752bbe

-- __unique:__ @test_edgecasesnames_Example_get_javascript@
foreign import ccall unsafe "hs_bindgen_3d349ab20e55d3d5" hs_bindgen_3d349ab20e55d3d5_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_javascript@
hs_bindgen_3d349ab20e55d3d5 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_3d349ab20e55d3d5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3d349ab20e55d3d5_base

{-# NOINLINE javascript_funptr #-}
{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h 21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript_funptr :: Ptr.FunPtr (IO ())
javascript_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3d349ab20e55d3d5

-- __unique:__ @test_edgecasesnames_Example_get_label@
foreign import ccall unsafe "hs_bindgen_7ab78b24e63738f5" hs_bindgen_7ab78b24e63738f5_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_label@
hs_bindgen_7ab78b24e63738f5 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_7ab78b24e63738f5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7ab78b24e63738f5_base

{-# NOINLINE label_funptr #-}
{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h 22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label_funptr :: Ptr.FunPtr (IO ())
label_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7ab78b24e63738f5

-- __unique:__ @test_edgecasesnames_Example_get_prim@
foreign import ccall unsafe "hs_bindgen_13caa66b81eebdfb" hs_bindgen_13caa66b81eebdfb_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_prim@
hs_bindgen_13caa66b81eebdfb :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_13caa66b81eebdfb =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_13caa66b81eebdfb_base

{-# NOINLINE prim_funptr #-}
{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h 23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim_funptr :: Ptr.FunPtr (IO ())
prim_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_13caa66b81eebdfb

-- __unique:__ @test_edgecasesnames_Example_get_role@
foreign import ccall unsafe "hs_bindgen_99189a38ba22fdad" hs_bindgen_99189a38ba22fdad_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_role@
hs_bindgen_99189a38ba22fdad :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_99189a38ba22fdad =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_99189a38ba22fdad_base

{-# NOINLINE role_funptr #-}
{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h 24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role_funptr :: Ptr.FunPtr (IO ())
role_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_99189a38ba22fdad

-- __unique:__ @test_edgecasesnames_Example_get_safe@
foreign import ccall unsafe "hs_bindgen_fe3819b9444e41b6" hs_bindgen_fe3819b9444e41b6_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_safe@
hs_bindgen_fe3819b9444e41b6 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_fe3819b9444e41b6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fe3819b9444e41b6_base

{-# NOINLINE safe_funptr #-}
{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h 25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe_funptr :: Ptr.FunPtr (IO ())
safe_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fe3819b9444e41b6

-- __unique:__ @test_edgecasesnames_Example_get_stdcall@
foreign import ccall unsafe "hs_bindgen_7d4e52bd26694f13" hs_bindgen_7d4e52bd26694f13_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_stdcall@
hs_bindgen_7d4e52bd26694f13 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_7d4e52bd26694f13 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7d4e52bd26694f13_base

{-# NOINLINE stdcall_funptr #-}
{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h 26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall_funptr :: Ptr.FunPtr (IO ())
stdcall_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d4e52bd26694f13

-- __unique:__ @test_edgecasesnames_Example_get_stock@
foreign import ccall unsafe "hs_bindgen_be2fd8368b1f85bc" hs_bindgen_be2fd8368b1f85bc_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_stock@
hs_bindgen_be2fd8368b1f85bc :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_be2fd8368b1f85bc =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_be2fd8368b1f85bc_base

{-# NOINLINE stock_funptr #-}
{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h 27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock_funptr :: Ptr.FunPtr (IO ())
stock_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_be2fd8368b1f85bc

-- __unique:__ @test_edgecasesnames_Example_get_unsafe@
foreign import ccall unsafe "hs_bindgen_d0faf7702a235ae1" hs_bindgen_d0faf7702a235ae1_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_unsafe@
hs_bindgen_d0faf7702a235ae1 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_d0faf7702a235ae1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d0faf7702a235ae1_base

{-# NOINLINE unsafe_funptr #-}
{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h 28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe_funptr :: Ptr.FunPtr (IO ())
unsafe_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d0faf7702a235ae1

-- __unique:__ @test_edgecasesnames_Example_get_via@
foreign import ccall unsafe "hs_bindgen_56fc690ff4bc7c4e" hs_bindgen_56fc690ff4bc7c4e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_edgecasesnames_Example_get_via@
hs_bindgen_56fc690ff4bc7c4e :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_56fc690ff4bc7c4e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_56fc690ff4bc7c4e_base

{-# NOINLINE via_funptr #-}
{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h 29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via_funptr :: Ptr.FunPtr (IO ())
via_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_56fc690ff4bc7c4e
