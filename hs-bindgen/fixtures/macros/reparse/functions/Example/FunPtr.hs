{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    , Example.FunPtr.baz
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_foo@
hs_bindgen_0ee6ccf5e9065e6c :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_0ee6ccf5e9065e6c =
  RIP.fromFFIType hs_bindgen_0ee6ccf5e9065e6c_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo :: RIP.FunPtr (RIP.CInt -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_0ee6ccf5e9065e6c

-- __unique:__ @test_macrosreparsefunctions_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_8ec5846013c7fa41" hs_bindgen_8ec5846013c7fa41_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_bar@
hs_bindgen_8ec5846013c7fa41 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_8ec5846013c7fa41 =
  RIP.fromFFIType hs_bindgen_8ec5846013c7fa41_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
bar = RIP.unsafePerformIO hs_bindgen_8ec5846013c7fa41

-- __unique:__ @test_macrosreparsefunctions_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_740f410e15c8820e" hs_bindgen_740f410e15c8820e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparsefunctions_Example_get_baz@
hs_bindgen_740f410e15c8820e :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_740f410e15c8820e =
  RIP.fromFFIType hs_bindgen_740f410e15c8820e_base

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
baz = RIP.unsafePerformIO hs_bindgen_740f410e15c8820e
