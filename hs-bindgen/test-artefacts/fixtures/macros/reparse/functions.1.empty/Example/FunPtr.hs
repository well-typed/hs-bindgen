{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    , Example.FunPtr.baz
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/functions.h>"
  , "/* test_macrosreparsefunctions_1_emp_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_44fe83b726d01723 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparsefunctions_1_emp_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_54a28d63fc0bcff0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_macrosreparsefunctions_1_emp_Example_get_baz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ae839294fa23e037 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_44fe83b726d01723" hs_bindgen_44fe83b726d01723_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_foo@
hs_bindgen_44fe83b726d01723 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_44fe83b726d01723 =
  BG.fromFFIType hs_bindgen_44fe83b726d01723_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_44fe83b726d01723

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_54a28d63fc0bcff0" hs_bindgen_54a28d63fc0bcff0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_bar@
hs_bindgen_54a28d63fc0bcff0 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_54a28d63fc0bcff0 =
  BG.fromFFIType hs_bindgen_54a28d63fc0bcff0_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar :: BG.FunPtr (BG.CInt -> IO BG.CInt)
bar = BG.unsafePerformIO hs_bindgen_54a28d63fc0bcff0

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_ae839294fa23e037" hs_bindgen_ae839294fa23e037_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_1_emp_Example_get_baz@
hs_bindgen_ae839294fa23e037 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_ae839294fa23e037 =
  BG.fromFFIType hs_bindgen_ae839294fa23e037_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz :: BG.FunPtr (BG.CInt -> IO BG.CInt)
baz = BG.unsafePerformIO hs_bindgen_ae839294fa23e037
