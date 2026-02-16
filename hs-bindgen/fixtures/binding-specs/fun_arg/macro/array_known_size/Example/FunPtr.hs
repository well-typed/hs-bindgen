{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array_known_size.h>"
  , "/* test_bindingspecsfun_argmacroar_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fc9d87cbca4127e4 (void)) ("
  , "  MyArray arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0dbc306c53c94679 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db0d885ae35d447f (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_fc9d87cbca4127e4" hs_bindgen_fc9d87cbca4127e4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_foo@
hs_bindgen_fc9d87cbca4127e4 :: IO (RIP.FunPtr (MyArray -> IO ()))
hs_bindgen_fc9d87cbca4127e4 =
  RIP.fromFFIType hs_bindgen_fc9d87cbca4127e4_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
foo :: RIP.FunPtr (MyArray -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_fc9d87cbca4127e4

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_0dbc306c53c94679" hs_bindgen_0dbc306c53c94679_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooA@
hs_bindgen_0dbc306c53c94679 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_0dbc306c53c94679 =
  RIP.fromFFIType hs_bindgen_0dbc306c53c94679_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_0dbc306c53c94679

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_db0d885ae35d447f" hs_bindgen_db0d885ae35d447f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooB@
hs_bindgen_db0d885ae35d447f :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_db0d885ae35d447f =
  RIP.fromFFIType hs_bindgen_db0d885ae35d447f_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_db0d885ae35d447f
