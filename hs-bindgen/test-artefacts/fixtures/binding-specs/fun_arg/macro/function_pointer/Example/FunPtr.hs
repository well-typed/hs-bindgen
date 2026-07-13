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

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function_pointer.h>"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e113ab1229902001 (void)) ("
  , "  MyFunctionPointer arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5e83648377d8afc6 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d6d79c737e65f7e7 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_566c44a793d222a9 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9214c66c2650a721 (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_08a6ea4ce7e2c244 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_e113ab1229902001" hs_bindgen_e113ab1229902001_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
hs_bindgen_e113ab1229902001 :: IO (BG.FunPtr (MyFunctionPointer -> IO ()))
hs_bindgen_e113ab1229902001 =
  BG.fromFFIType hs_bindgen_e113ab1229902001_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
foo :: BG.FunPtr (MyFunctionPointer -> IO ())
foo = BG.unsafePerformIO hs_bindgen_e113ab1229902001

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_5e83648377d8afc6" hs_bindgen_5e83648377d8afc6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
hs_bindgen_5e83648377d8afc6 :: IO (BG.FunPtr (A -> IO ()))
hs_bindgen_5e83648377d8afc6 =
  BG.fromFFIType hs_bindgen_5e83648377d8afc6_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooA :: BG.FunPtr (A -> IO ())
fooA = BG.unsafePerformIO hs_bindgen_5e83648377d8afc6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d6d79c737e65f7e7" hs_bindgen_d6d79c737e65f7e7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
hs_bindgen_d6d79c737e65f7e7 :: IO (BG.FunPtr (B -> IO ()))
hs_bindgen_d6d79c737e65f7e7 =
  BG.fromFFIType hs_bindgen_d6d79c737e65f7e7_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooB :: BG.FunPtr (B -> IO ())
fooB = BG.unsafePerformIO hs_bindgen_d6d79c737e65f7e7

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_566c44a793d222a9" hs_bindgen_566c44a793d222a9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooC@
hs_bindgen_566c44a793d222a9 :: IO (BG.FunPtr (M.C -> IO ()))
hs_bindgen_566c44a793d222a9 =
  BG.fromFFIType hs_bindgen_566c44a793d222a9_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooC :: BG.FunPtr (M.C -> IO ())
fooC = BG.unsafePerformIO hs_bindgen_566c44a793d222a9

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_9214c66c2650a721" hs_bindgen_9214c66c2650a721_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooD@
hs_bindgen_9214c66c2650a721 :: IO (BG.FunPtr (M.D -> IO ()))
hs_bindgen_9214c66c2650a721 =
  BG.fromFFIType hs_bindgen_9214c66c2650a721_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooD :: BG.FunPtr (M.D -> IO ())
fooD = BG.unsafePerformIO hs_bindgen_9214c66c2650a721

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_08a6ea4ce7e2c244" hs_bindgen_08a6ea4ce7e2c244_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooE@
hs_bindgen_08a6ea4ce7e2c244 :: IO (BG.FunPtr (E -> IO ()))
hs_bindgen_08a6ea4ce7e2c244 =
  BG.fromFFIType hs_bindgen_08a6ea4ce7e2c244_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooE :: BG.FunPtr (E -> IO ())
fooE = BG.unsafePerformIO hs_bindgen_08a6ea4ce7e2c244
