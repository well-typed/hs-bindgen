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
  , "/* test_macrosreparsefunctions_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0ee6ccf5e9065e6c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparsefunctions_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8ec5846013c7fa41 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_macrosreparsefunctions_Example_get_baz */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_740f410e15c8820e (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_0ee6ccf5e9065e6c" hs_bindgen_0ee6ccf5e9065e6c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_foo@
hs_bindgen_0ee6ccf5e9065e6c :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_0ee6ccf5e9065e6c =
  BG.fromFFIType hs_bindgen_0ee6ccf5e9065e6c_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_0ee6ccf5e9065e6c

-- __unique:__ @test_macrosreparsefunctions_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_8ec5846013c7fa41" hs_bindgen_8ec5846013c7fa41_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_bar@
hs_bindgen_8ec5846013c7fa41 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_8ec5846013c7fa41 =
  BG.fromFFIType hs_bindgen_8ec5846013c7fa41_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar :: BG.FunPtr (BG.CInt -> IO BG.CInt)
bar = BG.unsafePerformIO hs_bindgen_8ec5846013c7fa41

-- __unique:__ @test_macrosreparsefunctions_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_740f410e15c8820e" hs_bindgen_740f410e15c8820e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_baz@
hs_bindgen_740f410e15c8820e :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_740f410e15c8820e =
  BG.fromFFIType hs_bindgen_740f410e15c8820e_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz :: BG.FunPtr (BG.CInt -> IO BG.CInt)
baz = BG.unsafePerformIO hs_bindgen_740f410e15c8820e
