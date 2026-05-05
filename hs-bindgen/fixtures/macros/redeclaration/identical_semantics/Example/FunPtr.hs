{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/redeclaration/identical_semantics.h>"
  , "/* test_macrosredeclarationidentical_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a17f85783f80f294 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_macrosredeclarationidentical_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fbbde3e7da8ad667 (void)) ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_a17f85783f80f294" hs_bindgen_a17f85783f80f294_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationidentical_Example_get_foo@
hs_bindgen_a17f85783f80f294 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_a17f85783f80f294 =
  RIP.fromFFIType hs_bindgen_a17f85783f80f294_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 4:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
foo :: RIP.FunPtr (T -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_a17f85783f80f294

-- __unique:__ @test_macrosredeclarationidentical_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_fbbde3e7da8ad667" hs_bindgen_fbbde3e7da8ad667_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationidentical_Example_get_bar@
hs_bindgen_fbbde3e7da8ad667 :: IO (RIP.FunPtr (T -> IO ()))
hs_bindgen_fbbde3e7da8ad667 =
  RIP.fromFFIType hs_bindgen_fbbde3e7da8ad667_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 6:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
bar :: RIP.FunPtr (T -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_fbbde3e7da8ad667
