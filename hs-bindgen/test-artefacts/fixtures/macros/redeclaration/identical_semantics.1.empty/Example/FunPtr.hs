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
  [ "#include <macros/redeclaration/identical_semantics.h>"
  , "/* test_macrosredeclarationidentical_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a17f85783f80f294 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosredeclarationidentical_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fbbde3e7da8ad667 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_a17f85783f80f294" hs_bindgen_a17f85783f80f294_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationidentical_Example_get_foo@
hs_bindgen_a17f85783f80f294 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_a17f85783f80f294 =
  BG.fromFFIType hs_bindgen_a17f85783f80f294_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 3:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
foo :: BG.FunPtr (BG.CInt -> IO ())
foo = BG.unsafePerformIO hs_bindgen_a17f85783f80f294

-- __unique:__ @test_macrosredeclarationidentical_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_fbbde3e7da8ad667" hs_bindgen_fbbde3e7da8ad667_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosredeclarationidentical_Example_get_bar@
hs_bindgen_fbbde3e7da8ad667 :: IO (BG.FunPtr (BG.CInt -> IO ()))
hs_bindgen_fbbde3e7da8ad667 =
  BG.fromFFIType hs_bindgen_fbbde3e7da8ad667_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 5:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
bar :: BG.FunPtr (BG.CInt -> IO ())
bar = BG.unsafePerformIO hs_bindgen_fbbde3e7da8ad667
