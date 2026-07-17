{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_foo@
hs_bindgen_d0db0eb938233932 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_d0db0eb938233932 =
  BG.fromFFIType hs_bindgen_d0db0eb938233932_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/different.h 3:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_d0db0eb938233932

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_46700de9dec9ddc2" hs_bindgen_46700de9dec9ddc2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationdifferent_Example_get_bar@
hs_bindgen_46700de9dec9ddc2 :: IO (BG.FunPtr (BG.CChar -> IO ()))
hs_bindgen_46700de9dec9ddc2 =
  BG.fromFFIType hs_bindgen_46700de9dec9ddc2_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/different.h 5:6@

    __exported by:__ @macros\/redeclaration\/different.h@
-}
bar :: BG.FunPtr (BG.CChar -> IO ())
bar = BG.unsafePerformIO hs_bindgen_46700de9dec9ddc2
