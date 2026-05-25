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
  [ "#include <macros/redeclaration/different.h>"
  , "/* test_macrosredeclarationdifferent_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d0db0eb938233932 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosredeclarationdifferent_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_46700de9dec9ddc2 (void)) ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_d0db0eb938233932" hs_bindgen_d0db0eb938233932_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_foo@
hs_bindgen_d0db0eb938233932 :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_d0db0eb938233932 =
  RIP.fromFFIType hs_bindgen_d0db0eb938233932_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/different.h 4:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
foo :: RIP.FunPtr (RIP.CInt -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_d0db0eb938233932

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_46700de9dec9ddc2" hs_bindgen_46700de9dec9ddc2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_bar@
hs_bindgen_46700de9dec9ddc2 :: IO (RIP.FunPtr (RIP.CChar -> IO ()))
hs_bindgen_46700de9dec9ddc2 =
  RIP.fromFFIType hs_bindgen_46700de9dec9ddc2_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/different.h 6:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
bar :: RIP.FunPtr (RIP.CChar -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_46700de9dec9ddc2
