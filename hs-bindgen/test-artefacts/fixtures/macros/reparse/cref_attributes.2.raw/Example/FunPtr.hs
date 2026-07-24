{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    , Example.FunPtr.quux
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/cref_attributes.h>"
  , "/* test_macrosreparsecref_attributes_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_dfdc3c5362469496 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparsecref_attributes_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b44be06e46c9f5f3 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_macrosreparsecref_attributes_Example_get_quux */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f923b761933d67da (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_dfdc3c5362469496" hs_bindgen_dfdc3c5362469496_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_foo@
hs_bindgen_dfdc3c5362469496 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_dfdc3c5362469496 =
  BG.fromFFIType hs_bindgen_dfdc3c5362469496_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/cref_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_dfdc3c5362469496

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_b44be06e46c9f5f3" hs_bindgen_b44be06e46c9f5f3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_bar@
hs_bindgen_b44be06e46c9f5f3 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_b44be06e46c9f5f3 =
  BG.fromFFIType hs_bindgen_b44be06e46c9f5f3_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/cref_attributes.h 6:37@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
bar :: BG.FunPtr (BG.CInt -> IO ())
bar = BG.unsafePerformIO hs_bindgen_b44be06e46c9f5f3

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_quux@
foreign import ccall unsafe "hs_bindgen_f923b761933d67da" hs_bindgen_f923b761933d67da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsecref_attributes_Example_get_quux@
hs_bindgen_f923b761933d67da :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_f923b761933d67da =
  BG.fromFFIType hs_bindgen_f923b761933d67da_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/cref_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
quux :: BG.FunPtr (BG.CInt -> IO ())
quux = BG.unsafePerformIO hs_bindgen_f923b761933d67da
