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
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "/* test_bindingspecsfun_argmacrost_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ccfc23165c7fd4a9 (void)) ("
  , "  struct MyStruct arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ab74a4a30349b6b2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_19855bed49223360 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_11912ef040cef859 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_45e77aca55d2f794 (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d8f4dbc2ffc05918 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_ccfc23165c7fd4a9" hs_bindgen_ccfc23165c7fd4a9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_foo@
hs_bindgen_ccfc23165c7fd4a9 :: IO (RIP.FunPtr (MyStruct -> IO ()))
hs_bindgen_ccfc23165c7fd4a9 =
  RIP.fromFFIType hs_bindgen_ccfc23165c7fd4a9_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 8:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo :: RIP.FunPtr (MyStruct -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_ccfc23165c7fd4a9

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_ab74a4a30349b6b2" hs_bindgen_ab74a4a30349b6b2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
hs_bindgen_ab74a4a30349b6b2 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_ab74a4a30349b6b2 =
  RIP.fromFFIType hs_bindgen_ab74a4a30349b6b2_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 14:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_ab74a4a30349b6b2

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_19855bed49223360" hs_bindgen_19855bed49223360_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
hs_bindgen_19855bed49223360 :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_19855bed49223360 =
  RIP.fromFFIType hs_bindgen_19855bed49223360_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 15:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_19855bed49223360

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_11912ef040cef859" hs_bindgen_11912ef040cef859_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooC@
hs_bindgen_11912ef040cef859 :: IO (RIP.FunPtr (M.C -> IO ()))
hs_bindgen_11912ef040cef859 =
  RIP.fromFFIType hs_bindgen_11912ef040cef859_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooC :: RIP.FunPtr (M.C -> IO ())
fooC =
  RIP.unsafePerformIO hs_bindgen_11912ef040cef859

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_45e77aca55d2f794" hs_bindgen_45e77aca55d2f794_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooD@
hs_bindgen_45e77aca55d2f794 :: IO (RIP.FunPtr (M.D -> IO ()))
hs_bindgen_45e77aca55d2f794 =
  RIP.fromFFIType hs_bindgen_45e77aca55d2f794_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 38:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooD :: RIP.FunPtr (M.D -> IO ())
fooD =
  RIP.unsafePerformIO hs_bindgen_45e77aca55d2f794

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_d8f4dbc2ffc05918" hs_bindgen_d8f4dbc2ffc05918_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooE@
hs_bindgen_d8f4dbc2ffc05918 :: IO (RIP.FunPtr (E -> IO ()))
hs_bindgen_d8f4dbc2ffc05918 =
  RIP.fromFFIType hs_bindgen_d8f4dbc2ffc05918_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 39:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooE :: RIP.FunPtr (E -> IO ())
fooE =
  RIP.unsafePerformIO hs_bindgen_d8f4dbc2ffc05918
