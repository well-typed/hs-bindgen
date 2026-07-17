{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.by'
    , Example.FunPtr.forall'
    , Example.FunPtr.mdo'
    , Example.FunPtr.pattern'
    , Example.FunPtr.proc'
    , Example.FunPtr.rec'
    , Example.FunPtr.using'
    , Example.FunPtr.anyclass
    , Example.FunPtr.capi
    , Example.FunPtr.cases
    , Example.FunPtr.ccall
    , Example.FunPtr.dynamic
    , Example.FunPtr.export
    , Example.FunPtr.family
    , Example.FunPtr.group
    , Example.FunPtr.interruptible
    , Example.FunPtr.javascript
    , Example.FunPtr.label
    , Example.FunPtr.prim
    , Example.FunPtr.role
    , Example.FunPtr.safe
    , Example.FunPtr.stdcall
    , Example.FunPtr.stock
    , Example.FunPtr.unsafe
    , Example.FunPtr.via
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_by@
hs_bindgen_d50b20f504174dd5 :: IO (BG.FunPtr (IO ()))
hs_bindgen_d50b20f504174dd5 =
  BG.fromFFIType hs_bindgen_d50b20f504174dd5_base

{-# NOINLINE by' #-}
{-| __C declaration:__ @by@

    __defined at:__ @edge-cases\/names.h 3:6@

    __exported by:__ @edge-cases\/names.h@
-}
by' :: BG.FunPtr (IO ())
by' = BG.unsafePerformIO hs_bindgen_d50b20f504174dd5

-- __unique:__ @test_edgecasesnames_Example_get_forall@
foreign import ccall unsafe "hs_bindgen_f7e660ea9223a909" hs_bindgen_f7e660ea9223a909_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_forall@
hs_bindgen_f7e660ea9223a909 :: IO (BG.FunPtr (IO ()))
hs_bindgen_f7e660ea9223a909 =
  BG.fromFFIType hs_bindgen_f7e660ea9223a909_base

{-# NOINLINE forall' #-}
{-| __C declaration:__ @forall@

    __defined at:__ @edge-cases\/names.h 4:6@

    __exported by:__ @edge-cases\/names.h@
-}
forall' :: BG.FunPtr (IO ())
forall' =
  BG.unsafePerformIO hs_bindgen_f7e660ea9223a909

-- __unique:__ @test_edgecasesnames_Example_get_mdo@
foreign import ccall unsafe "hs_bindgen_15319d1b8d2ca927" hs_bindgen_15319d1b8d2ca927_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_mdo@
hs_bindgen_15319d1b8d2ca927 :: IO (BG.FunPtr (IO ()))
hs_bindgen_15319d1b8d2ca927 =
  BG.fromFFIType hs_bindgen_15319d1b8d2ca927_base

{-# NOINLINE mdo' #-}
{-| __C declaration:__ @mdo@

    __defined at:__ @edge-cases\/names.h 5:6@

    __exported by:__ @edge-cases\/names.h@
-}
mdo' :: BG.FunPtr (IO ())
mdo' = BG.unsafePerformIO hs_bindgen_15319d1b8d2ca927

-- __unique:__ @test_edgecasesnames_Example_get_pattern@
foreign import ccall unsafe "hs_bindgen_e5a1961c11d641ed" hs_bindgen_e5a1961c11d641ed_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_pattern@
hs_bindgen_e5a1961c11d641ed :: IO (BG.FunPtr (IO ()))
hs_bindgen_e5a1961c11d641ed =
  BG.fromFFIType hs_bindgen_e5a1961c11d641ed_base

{-# NOINLINE pattern' #-}
{-| __C declaration:__ @pattern@

    __defined at:__ @edge-cases\/names.h 6:6@

    __exported by:__ @edge-cases\/names.h@
-}
pattern' :: BG.FunPtr (IO ())
pattern' =
  BG.unsafePerformIO hs_bindgen_e5a1961c11d641ed

-- __unique:__ @test_edgecasesnames_Example_get_proc@
foreign import ccall unsafe "hs_bindgen_d2dd74adbb4a606c" hs_bindgen_d2dd74adbb4a606c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_proc@
hs_bindgen_d2dd74adbb4a606c :: IO (BG.FunPtr (IO ()))
hs_bindgen_d2dd74adbb4a606c =
  BG.fromFFIType hs_bindgen_d2dd74adbb4a606c_base

{-# NOINLINE proc' #-}
{-| __C declaration:__ @proc@

    __defined at:__ @edge-cases\/names.h 7:6@

    __exported by:__ @edge-cases\/names.h@
-}
proc' :: BG.FunPtr (IO ())
proc' =
  BG.unsafePerformIO hs_bindgen_d2dd74adbb4a606c

-- __unique:__ @test_edgecasesnames_Example_get_rec@
foreign import ccall unsafe "hs_bindgen_bab6d9c9540c9d58" hs_bindgen_bab6d9c9540c9d58_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_rec@
hs_bindgen_bab6d9c9540c9d58 :: IO (BG.FunPtr (IO ()))
hs_bindgen_bab6d9c9540c9d58 =
  BG.fromFFIType hs_bindgen_bab6d9c9540c9d58_base

{-# NOINLINE rec' #-}
{-| __C declaration:__ @rec@

    __defined at:__ @edge-cases\/names.h 8:6@

    __exported by:__ @edge-cases\/names.h@
-}
rec' :: BG.FunPtr (IO ())
rec' = BG.unsafePerformIO hs_bindgen_bab6d9c9540c9d58

-- __unique:__ @test_edgecasesnames_Example_get_using@
foreign import ccall unsafe "hs_bindgen_447fa7223c86f5c7" hs_bindgen_447fa7223c86f5c7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_using@
hs_bindgen_447fa7223c86f5c7 :: IO (BG.FunPtr (IO ()))
hs_bindgen_447fa7223c86f5c7 =
  BG.fromFFIType hs_bindgen_447fa7223c86f5c7_base

{-# NOINLINE using' #-}
{-| __C declaration:__ @using@

    __defined at:__ @edge-cases\/names.h 9:6@

    __exported by:__ @edge-cases\/names.h@
-}
using' :: BG.FunPtr (IO ())
using' =
  BG.unsafePerformIO hs_bindgen_447fa7223c86f5c7

-- __unique:__ @test_edgecasesnames_Example_get_anyclass@
foreign import ccall unsafe "hs_bindgen_f2292c17facdbcde" hs_bindgen_f2292c17facdbcde_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_anyclass@
hs_bindgen_f2292c17facdbcde :: IO (BG.FunPtr (IO ()))
hs_bindgen_f2292c17facdbcde =
  BG.fromFFIType hs_bindgen_f2292c17facdbcde_base

{-# NOINLINE anyclass #-}
{-| __C declaration:__ @anyclass@

    __defined at:__ @edge-cases\/names.h 12:6@

    __exported by:__ @edge-cases\/names.h@
-}
anyclass :: BG.FunPtr (IO ())
anyclass =
  BG.unsafePerformIO hs_bindgen_f2292c17facdbcde

-- __unique:__ @test_edgecasesnames_Example_get_capi@
foreign import ccall unsafe "hs_bindgen_393b62c1a821de61" hs_bindgen_393b62c1a821de61_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_capi@
hs_bindgen_393b62c1a821de61 :: IO (BG.FunPtr (IO ()))
hs_bindgen_393b62c1a821de61 =
  BG.fromFFIType hs_bindgen_393b62c1a821de61_base

{-# NOINLINE capi #-}
{-| __C declaration:__ @capi@

    __defined at:__ @edge-cases\/names.h 13:6@

    __exported by:__ @edge-cases\/names.h@
-}
capi :: BG.FunPtr (IO ())
capi = BG.unsafePerformIO hs_bindgen_393b62c1a821de61

-- __unique:__ @test_edgecasesnames_Example_get_cases@
foreign import ccall unsafe "hs_bindgen_962b5bf9813ea6dd" hs_bindgen_962b5bf9813ea6dd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_cases@
hs_bindgen_962b5bf9813ea6dd :: IO (BG.FunPtr (IO ()))
hs_bindgen_962b5bf9813ea6dd =
  BG.fromFFIType hs_bindgen_962b5bf9813ea6dd_base

{-# NOINLINE cases #-}
{-| __C declaration:__ @cases@

    __defined at:__ @edge-cases\/names.h 14:6@

    __exported by:__ @edge-cases\/names.h@
-}
cases :: BG.FunPtr (IO ())
cases =
  BG.unsafePerformIO hs_bindgen_962b5bf9813ea6dd

-- __unique:__ @test_edgecasesnames_Example_get_ccall@
foreign import ccall unsafe "hs_bindgen_599948844183b4ea" hs_bindgen_599948844183b4ea_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_ccall@
hs_bindgen_599948844183b4ea :: IO (BG.FunPtr (IO ()))
hs_bindgen_599948844183b4ea =
  BG.fromFFIType hs_bindgen_599948844183b4ea_base

{-# NOINLINE ccall #-}
{-| __C declaration:__ @ccall@

    __defined at:__ @edge-cases\/names.h 15:6@

    __exported by:__ @edge-cases\/names.h@
-}
ccall :: BG.FunPtr (IO ())
ccall =
  BG.unsafePerformIO hs_bindgen_599948844183b4ea

-- __unique:__ @test_edgecasesnames_Example_get_dynamic@
foreign import ccall unsafe "hs_bindgen_aa442ea764d9d93c" hs_bindgen_aa442ea764d9d93c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_dynamic@
hs_bindgen_aa442ea764d9d93c :: IO (BG.FunPtr (IO ()))
hs_bindgen_aa442ea764d9d93c =
  BG.fromFFIType hs_bindgen_aa442ea764d9d93c_base

{-# NOINLINE dynamic #-}
{-| __C declaration:__ @dynamic@

    __defined at:__ @edge-cases\/names.h 16:6@

    __exported by:__ @edge-cases\/names.h@
-}
dynamic :: BG.FunPtr (IO ())
dynamic =
  BG.unsafePerformIO hs_bindgen_aa442ea764d9d93c

-- __unique:__ @test_edgecasesnames_Example_get_export@
foreign import ccall unsafe "hs_bindgen_98dfb21e9f7b6857" hs_bindgen_98dfb21e9f7b6857_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_export@
hs_bindgen_98dfb21e9f7b6857 :: IO (BG.FunPtr (IO ()))
hs_bindgen_98dfb21e9f7b6857 =
  BG.fromFFIType hs_bindgen_98dfb21e9f7b6857_base

{-# NOINLINE export #-}
{-| __C declaration:__ @export@

    __defined at:__ @edge-cases\/names.h 17:6@

    __exported by:__ @edge-cases\/names.h@
-}
export :: BG.FunPtr (IO ())
export =
  BG.unsafePerformIO hs_bindgen_98dfb21e9f7b6857

-- __unique:__ @test_edgecasesnames_Example_get_family@
foreign import ccall unsafe "hs_bindgen_b96b9fd3fa689c75" hs_bindgen_b96b9fd3fa689c75_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_family@
hs_bindgen_b96b9fd3fa689c75 :: IO (BG.FunPtr (IO ()))
hs_bindgen_b96b9fd3fa689c75 =
  BG.fromFFIType hs_bindgen_b96b9fd3fa689c75_base

{-# NOINLINE family #-}
{-| __C declaration:__ @family@

    __defined at:__ @edge-cases\/names.h 18:6@

    __exported by:__ @edge-cases\/names.h@
-}
family :: BG.FunPtr (IO ())
family =
  BG.unsafePerformIO hs_bindgen_b96b9fd3fa689c75

-- __unique:__ @test_edgecasesnames_Example_get_group@
foreign import ccall unsafe "hs_bindgen_a4413aed06e03d16" hs_bindgen_a4413aed06e03d16_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_group@
hs_bindgen_a4413aed06e03d16 :: IO (BG.FunPtr (IO ()))
hs_bindgen_a4413aed06e03d16 =
  BG.fromFFIType hs_bindgen_a4413aed06e03d16_base

{-# NOINLINE group #-}
{-| __C declaration:__ @group@

    __defined at:__ @edge-cases\/names.h 19:6@

    __exported by:__ @edge-cases\/names.h@
-}
group :: BG.FunPtr (IO ())
group =
  BG.unsafePerformIO hs_bindgen_a4413aed06e03d16

-- __unique:__ @test_edgecasesnames_Example_get_interruptible@
foreign import ccall unsafe "hs_bindgen_52223789a2752bbe" hs_bindgen_52223789a2752bbe_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_interruptible@
hs_bindgen_52223789a2752bbe :: IO (BG.FunPtr (IO ()))
hs_bindgen_52223789a2752bbe =
  BG.fromFFIType hs_bindgen_52223789a2752bbe_base

{-# NOINLINE interruptible #-}
{-| __C declaration:__ @interruptible@

    __defined at:__ @edge-cases\/names.h 20:6@

    __exported by:__ @edge-cases\/names.h@
-}
interruptible :: BG.FunPtr (IO ())
interruptible =
  BG.unsafePerformIO hs_bindgen_52223789a2752bbe

-- __unique:__ @test_edgecasesnames_Example_get_javascript@
foreign import ccall unsafe "hs_bindgen_3d349ab20e55d3d5" hs_bindgen_3d349ab20e55d3d5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_javascript@
hs_bindgen_3d349ab20e55d3d5 :: IO (BG.FunPtr (IO ()))
hs_bindgen_3d349ab20e55d3d5 =
  BG.fromFFIType hs_bindgen_3d349ab20e55d3d5_base

{-# NOINLINE javascript #-}
{-| __C declaration:__ @javascript@

    __defined at:__ @edge-cases\/names.h 21:6@

    __exported by:__ @edge-cases\/names.h@
-}
javascript :: BG.FunPtr (IO ())
javascript =
  BG.unsafePerformIO hs_bindgen_3d349ab20e55d3d5

-- __unique:__ @test_edgecasesnames_Example_get_label@
foreign import ccall unsafe "hs_bindgen_7ab78b24e63738f5" hs_bindgen_7ab78b24e63738f5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_label@
hs_bindgen_7ab78b24e63738f5 :: IO (BG.FunPtr (IO ()))
hs_bindgen_7ab78b24e63738f5 =
  BG.fromFFIType hs_bindgen_7ab78b24e63738f5_base

{-# NOINLINE label #-}
{-| __C declaration:__ @label@

    __defined at:__ @edge-cases\/names.h 22:6@

    __exported by:__ @edge-cases\/names.h@
-}
label :: BG.FunPtr (IO ())
label =
  BG.unsafePerformIO hs_bindgen_7ab78b24e63738f5

-- __unique:__ @test_edgecasesnames_Example_get_prim@
foreign import ccall unsafe "hs_bindgen_13caa66b81eebdfb" hs_bindgen_13caa66b81eebdfb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_prim@
hs_bindgen_13caa66b81eebdfb :: IO (BG.FunPtr (IO ()))
hs_bindgen_13caa66b81eebdfb =
  BG.fromFFIType hs_bindgen_13caa66b81eebdfb_base

{-# NOINLINE prim #-}
{-| __C declaration:__ @prim@

    __defined at:__ @edge-cases\/names.h 23:6@

    __exported by:__ @edge-cases\/names.h@
-}
prim :: BG.FunPtr (IO ())
prim = BG.unsafePerformIO hs_bindgen_13caa66b81eebdfb

-- __unique:__ @test_edgecasesnames_Example_get_role@
foreign import ccall unsafe "hs_bindgen_99189a38ba22fdad" hs_bindgen_99189a38ba22fdad_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_role@
hs_bindgen_99189a38ba22fdad :: IO (BG.FunPtr (IO ()))
hs_bindgen_99189a38ba22fdad =
  BG.fromFFIType hs_bindgen_99189a38ba22fdad_base

{-# NOINLINE role #-}
{-| __C declaration:__ @role@

    __defined at:__ @edge-cases\/names.h 24:6@

    __exported by:__ @edge-cases\/names.h@
-}
role :: BG.FunPtr (IO ())
role = BG.unsafePerformIO hs_bindgen_99189a38ba22fdad

-- __unique:__ @test_edgecasesnames_Example_get_safe@
foreign import ccall unsafe "hs_bindgen_fe3819b9444e41b6" hs_bindgen_fe3819b9444e41b6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_safe@
hs_bindgen_fe3819b9444e41b6 :: IO (BG.FunPtr (IO ()))
hs_bindgen_fe3819b9444e41b6 =
  BG.fromFFIType hs_bindgen_fe3819b9444e41b6_base

{-# NOINLINE safe #-}
{-| __C declaration:__ @safe@

    __defined at:__ @edge-cases\/names.h 25:6@

    __exported by:__ @edge-cases\/names.h@
-}
safe :: BG.FunPtr (IO ())
safe = BG.unsafePerformIO hs_bindgen_fe3819b9444e41b6

-- __unique:__ @test_edgecasesnames_Example_get_stdcall@
foreign import ccall unsafe "hs_bindgen_7d4e52bd26694f13" hs_bindgen_7d4e52bd26694f13_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_stdcall@
hs_bindgen_7d4e52bd26694f13 :: IO (BG.FunPtr (IO ()))
hs_bindgen_7d4e52bd26694f13 =
  BG.fromFFIType hs_bindgen_7d4e52bd26694f13_base

{-# NOINLINE stdcall #-}
{-| __C declaration:__ @stdcall@

    __defined at:__ @edge-cases\/names.h 26:6@

    __exported by:__ @edge-cases\/names.h@
-}
stdcall :: BG.FunPtr (IO ())
stdcall =
  BG.unsafePerformIO hs_bindgen_7d4e52bd26694f13

-- __unique:__ @test_edgecasesnames_Example_get_stock@
foreign import ccall unsafe "hs_bindgen_be2fd8368b1f85bc" hs_bindgen_be2fd8368b1f85bc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_stock@
hs_bindgen_be2fd8368b1f85bc :: IO (BG.FunPtr (IO ()))
hs_bindgen_be2fd8368b1f85bc =
  BG.fromFFIType hs_bindgen_be2fd8368b1f85bc_base

{-# NOINLINE stock #-}
{-| __C declaration:__ @stock@

    __defined at:__ @edge-cases\/names.h 27:6@

    __exported by:__ @edge-cases\/names.h@
-}
stock :: BG.FunPtr (IO ())
stock =
  BG.unsafePerformIO hs_bindgen_be2fd8368b1f85bc

-- __unique:__ @test_edgecasesnames_Example_get_unsafe@
foreign import ccall unsafe "hs_bindgen_d0faf7702a235ae1" hs_bindgen_d0faf7702a235ae1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_unsafe@
hs_bindgen_d0faf7702a235ae1 :: IO (BG.FunPtr (IO ()))
hs_bindgen_d0faf7702a235ae1 =
  BG.fromFFIType hs_bindgen_d0faf7702a235ae1_base

{-# NOINLINE unsafe #-}
{-| __C declaration:__ @unsafe@

    __defined at:__ @edge-cases\/names.h 28:6@

    __exported by:__ @edge-cases\/names.h@
-}
unsafe :: BG.FunPtr (IO ())
unsafe =
  BG.unsafePerformIO hs_bindgen_d0faf7702a235ae1

-- __unique:__ @test_edgecasesnames_Example_get_via@
foreign import ccall unsafe "hs_bindgen_56fc690ff4bc7c4e" hs_bindgen_56fc690ff4bc7c4e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_edgecasesnames_Example_get_via@
hs_bindgen_56fc690ff4bc7c4e :: IO (BG.FunPtr (IO ()))
hs_bindgen_56fc690ff4bc7c4e =
  BG.fromFFIType hs_bindgen_56fc690ff4bc7c4e_base

{-# NOINLINE via #-}
{-| __C declaration:__ @via@

    __defined at:__ @edge-cases\/names.h 29:6@

    __exported by:__ @edge-cases\/names.h@
-}
via :: BG.FunPtr (IO ())
via = BG.unsafePerformIO hs_bindgen_56fc690ff4bc7c4e
