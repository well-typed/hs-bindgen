{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.fooA
    , Example.FunPtr.fooB
    , Example.FunPtr.fooC
    , Example.FunPtr.fooD
    , Example.FunPtr.fooE
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/array_known_size.h>"
  , "/* test_bindingspecsfun_argmacroar_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fc9d87cbca4127e4 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0dbc306c53c94679 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_db0d885ae35d447f (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_82d6fdd972810bf4 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_398c495511ffe384 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacroar_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b74f6af23eb996c3 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_fc9d87cbca4127e4" hs_bindgen_fc9d87cbca4127e4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_foo@
hs_bindgen_fc9d87cbca4127e4 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem MyArray) -> IO ()))
hs_bindgen_fc9d87cbca4127e4 =
  RIP.fromFFIType hs_bindgen_fc9d87cbca4127e4_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
foo :: RIP.FunPtr (RIP.Ptr (IsA.Elem MyArray) -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_fc9d87cbca4127e4

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_0dbc306c53c94679" hs_bindgen_0dbc306c53c94679_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooA@
hs_bindgen_0dbc306c53c94679 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem A) -> IO ()))
hs_bindgen_0dbc306c53c94679 =
  RIP.fromFFIType hs_bindgen_0dbc306c53c94679_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooA :: RIP.FunPtr (RIP.Ptr (IsA.Elem A) -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_0dbc306c53c94679

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_db0d885ae35d447f" hs_bindgen_db0d885ae35d447f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooB@
hs_bindgen_db0d885ae35d447f :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem B) -> IO ()))
hs_bindgen_db0d885ae35d447f =
  RIP.fromFFIType hs_bindgen_db0d885ae35d447f_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooB :: RIP.FunPtr (RIP.Ptr (IsA.Elem B) -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_db0d885ae35d447f

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_82d6fdd972810bf4" hs_bindgen_82d6fdd972810bf4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooC@
hs_bindgen_82d6fdd972810bf4 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem M.C) -> IO ()))
hs_bindgen_82d6fdd972810bf4 =
  RIP.fromFFIType hs_bindgen_82d6fdd972810bf4_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooC :: RIP.FunPtr (RIP.Ptr (IsA.Elem M.C) -> IO ())
fooC =
  RIP.unsafePerformIO hs_bindgen_82d6fdd972810bf4

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_398c495511ffe384" hs_bindgen_398c495511ffe384_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooD@
hs_bindgen_398c495511ffe384 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem M.D) -> IO ()))
hs_bindgen_398c495511ffe384 =
  RIP.fromFFIType hs_bindgen_398c495511ffe384_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooD :: RIP.FunPtr (RIP.Ptr (IsA.Elem M.D) -> IO ())
fooD =
  RIP.unsafePerformIO hs_bindgen_398c495511ffe384

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_b74f6af23eb996c3" hs_bindgen_b74f6af23eb996c3_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooE@
hs_bindgen_b74f6af23eb996c3 :: IO (RIP.FunPtr (RIP.Ptr (IsA.Elem E) -> IO ()))
hs_bindgen_b74f6af23eb996c3 =
  RIP.fromFFIType hs_bindgen_b74f6af23eb996c3_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array_known_size.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array_known_size.h@
-}
fooE :: RIP.FunPtr (RIP.Ptr (IsA.Elem E) -> IO ())
fooE =
  RIP.unsafePerformIO hs_bindgen_b74f6af23eb996c3
