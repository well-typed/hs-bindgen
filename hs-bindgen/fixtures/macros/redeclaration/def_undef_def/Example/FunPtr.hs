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

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_foo@
hs_bindgen_cf4e7a02d3afe48b :: IO (RIP.FunPtr (RIP.CInt -> IO ()))
hs_bindgen_cf4e7a02d3afe48b =
  RIP.fromFFIType hs_bindgen_cf4e7a02d3afe48b_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 4:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
foo :: RIP.FunPtr (RIP.CInt -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_cf4e7a02d3afe48b

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_0d437e8c57fae439" hs_bindgen_0d437e8c57fae439_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosredeclarationdef_undef_Example_get_bar@
hs_bindgen_0d437e8c57fae439 :: IO (RIP.FunPtr (RIP.CChar -> IO ()))
hs_bindgen_0d437e8c57fae439 =
  RIP.fromFFIType hs_bindgen_0d437e8c57fae439_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/def_undef_def.h 7:6@

    __exported by:__ @macros\/redeclaration\/def_undef_def.h@
-}
bar :: RIP.FunPtr (RIP.CChar -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_0d437e8c57fae439
