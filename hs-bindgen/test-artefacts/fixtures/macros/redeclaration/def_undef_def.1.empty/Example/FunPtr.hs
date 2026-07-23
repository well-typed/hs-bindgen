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
  [ "#include <macros/redeclaration/def_undef_def.h>"
  , "/* test_macrosredeclarationdef_undef_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cf4e7a02d3afe48b (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosredeclarationdef_undef_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0d437e8c57fae439 (void)) ("
  , "  char arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_cf4e7a02d3afe48b" hs_bindgen_cf4e7a02d3afe48b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_foo@
hs_bindgen_cf4e7a02d3afe48b :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_cf4e7a02d3afe48b =
  BG.fromFFIType hs_bindgen_cf4e7a02d3afe48b_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 4:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_cf4e7a02d3afe48b

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_0d437e8c57fae439" hs_bindgen_0d437e8c57fae439_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_bar@
hs_bindgen_0d437e8c57fae439 :: IO (BG.FunPtr (BG.CChar -> IO ()))
hs_bindgen_0d437e8c57fae439 =
  BG.fromFFIType hs_bindgen_0d437e8c57fae439_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 7:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
bar :: BG.FunPtr (BG.CChar -> IO ())
bar = BG.unsafePerformIO hs_bindgen_0d437e8c57fae439
