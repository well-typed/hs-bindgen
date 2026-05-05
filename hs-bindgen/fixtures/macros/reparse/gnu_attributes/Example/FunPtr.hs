{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    , Example.FunPtr.dash
    , Example.FunPtr.quux
    , Example.FunPtr.heq
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "/* test_macrosreparsegnu_attributes_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_82e23f370cc161df (void)) ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparsegnu_attributes_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_30ffbacf53832dc4 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_macrosreparsegnu_attributes_Example_get_dash */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3cc756628379222d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &dash;"
  , "}"
  , "/* test_macrosreparsegnu_attributes_Example_get_quux */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c4a1e50d1013ab7d (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* test_macrosreparsegnu_attributes_Example_get_heq */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_646f1f9d97aa3318 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &heq;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_82e23f370cc161df" hs_bindgen_82e23f370cc161df_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_foo@
hs_bindgen_82e23f370cc161df :: IO (RIP.FunPtr (BOOL -> IO ()))
hs_bindgen_82e23f370cc161df =
  RIP.fromFFIType hs_bindgen_82e23f370cc161df_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo :: RIP.FunPtr (BOOL -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_82e23f370cc161df

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_30ffbacf53832dc4" hs_bindgen_30ffbacf53832dc4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_bar@
hs_bindgen_30ffbacf53832dc4 :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_30ffbacf53832dc4 =
  RIP.fromFFIType hs_bindgen_30ffbacf53832dc4_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar :: RIP.FunPtr (RIP.CInt -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_30ffbacf53832dc4

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_dash@
foreign import ccall unsafe "hs_bindgen_3cc756628379222d" hs_bindgen_3cc756628379222d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_dash@
hs_bindgen_3cc756628379222d :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_3cc756628379222d =
  RIP.fromFFIType hs_bindgen_3cc756628379222d_base

{-# NOINLINE dash #-}
{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash :: RIP.FunPtr (RIP.CInt -> IO ())
dash =
  RIP.unsafePerformIO hs_bindgen_3cc756628379222d

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_quux@
foreign import ccall unsafe "hs_bindgen_c4a1e50d1013ab7d" hs_bindgen_c4a1e50d1013ab7d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_quux@
hs_bindgen_c4a1e50d1013ab7d :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_c4a1e50d1013ab7d =
  RIP.fromFFIType hs_bindgen_c4a1e50d1013ab7d_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux :: RIP.FunPtr (RIP.CInt -> IO ())
quux =
  RIP.unsafePerformIO hs_bindgen_c4a1e50d1013ab7d

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_heq@
foreign import ccall unsafe "hs_bindgen_646f1f9d97aa3318" hs_bindgen_646f1f9d97aa3318_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsegnu_attributes_Example_get_heq@
hs_bindgen_646f1f9d97aa3318 :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_646f1f9d97aa3318 =
  RIP.fromFFIType hs_bindgen_646f1f9d97aa3318_base

{-# NOINLINE heq #-}
{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq :: RIP.FunPtr (RIP.CInt -> IO ())
heq = RIP.unsafePerformIO hs_bindgen_646f1f9d97aa3318
