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
  [ "#include <macros/reparse_functions.h>"
  , "/* test_macrosreparse_functions_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_083e29b1db19da1c (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosreparse_functions_Example_get_bar */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_42282c7e47b4f3c0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_functions_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_083e29b1db19da1c" hs_bindgen_083e29b1db19da1c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_functions_Example_get_foo@
hs_bindgen_083e29b1db19da1c :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_083e29b1db19da1c =
  RIP.fromFFIType hs_bindgen_083e29b1db19da1c_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse_functions.h 6:6@

    __exported by:__ @macros\/reparse_functions.h@
-}
foo :: RIP.FunPtr (RIP.CInt -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_083e29b1db19da1c

-- __unique:__ @test_macrosreparse_functions_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_42282c7e47b4f3c0" hs_bindgen_42282c7e47b4f3c0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosreparse_functions_Example_get_bar@
hs_bindgen_42282c7e47b4f3c0 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_42282c7e47b4f3c0 =
  RIP.fromFFIType hs_bindgen_42282c7e47b4f3c0_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse_functions.h 11:19@

    __exported by:__ @macros\/reparse_functions.h@
-}
bar :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
bar = RIP.unsafePerformIO hs_bindgen_42282c7e47b4f3c0
