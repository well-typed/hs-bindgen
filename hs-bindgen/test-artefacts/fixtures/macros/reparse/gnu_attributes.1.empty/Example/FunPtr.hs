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

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "/* test_macrosreparsegnu_attributes__Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0bb0e3098bde7e0b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparsegnu_attributes__Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b1dbc650a21707a2 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_macrosreparsegnu_attributes__Example_get_dash */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_edc10f73f46266b8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &dash;"
  , "}"
  , "/* test_macrosreparsegnu_attributes__Example_get_quux */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_50fdcccdf0a43b92 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* test_macrosreparsegnu_attributes__Example_get_heq */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_110df13ac4d9dfc8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &heq;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_foo@
foreign import ccall unsafe "hs_bindgen_0bb0e3098bde7e0b" hs_bindgen_0bb0e3098bde7e0b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_foo@
hs_bindgen_0bb0e3098bde7e0b :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_0bb0e3098bde7e0b =
  BG.fromFFIType hs_bindgen_0bb0e3098bde7e0b_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_0bb0e3098bde7e0b

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_bar@
foreign import ccall unsafe "hs_bindgen_b1dbc650a21707a2" hs_bindgen_b1dbc650a21707a2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_bar@
hs_bindgen_b1dbc650a21707a2 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_b1dbc650a21707a2 =
  BG.fromFFIType hs_bindgen_b1dbc650a21707a2_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar :: BG.FunPtr (BG.CInt -> IO ())
bar = BG.unsafePerformIO hs_bindgen_b1dbc650a21707a2

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_dash@
foreign import ccall unsafe "hs_bindgen_edc10f73f46266b8" hs_bindgen_edc10f73f46266b8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_dash@
hs_bindgen_edc10f73f46266b8 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_edc10f73f46266b8 =
  BG.fromFFIType hs_bindgen_edc10f73f46266b8_base

{-# NOINLINE dash #-}
{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash :: BG.FunPtr (BG.CInt -> IO ())
dash = BG.unsafePerformIO hs_bindgen_edc10f73f46266b8

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_quux@
foreign import ccall unsafe "hs_bindgen_50fdcccdf0a43b92" hs_bindgen_50fdcccdf0a43b92_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_quux@
hs_bindgen_50fdcccdf0a43b92 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_50fdcccdf0a43b92 =
  BG.fromFFIType hs_bindgen_50fdcccdf0a43b92_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux :: BG.FunPtr (BG.CInt -> IO ())
quux = BG.unsafePerformIO hs_bindgen_50fdcccdf0a43b92

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_heq@
foreign import ccall unsafe "hs_bindgen_110df13ac4d9dfc8" hs_bindgen_110df13ac4d9dfc8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsegnu_attributes__Example_get_heq@
hs_bindgen_110df13ac4d9dfc8 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_110df13ac4d9dfc8 =
  BG.fromFFIType hs_bindgen_110df13ac4d9dfc8_base

{-# NOINLINE heq #-}
{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq :: BG.FunPtr (BG.CInt -> IO ())
heq = BG.unsafePerformIO hs_bindgen_110df13ac4d9dfc8
