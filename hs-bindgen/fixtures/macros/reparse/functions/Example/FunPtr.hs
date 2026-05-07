{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
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

    __defined at:__ @macros\/reparse\/functions.h 6:6@

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

    __defined at:__ @macros\/reparse\/functions.h 11:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
bar = RIP.unsafePerformIO hs_bindgen_8ec5846013c7fa41
