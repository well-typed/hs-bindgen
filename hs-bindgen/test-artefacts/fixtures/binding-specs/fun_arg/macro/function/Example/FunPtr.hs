{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.fooA
    , Example.FunPtr.fooB
    , Example.FunPtr.fooC
    , Example.FunPtr.fooD
    , Example.FunPtr.fooE
    , Example.FunPtr.bar
    , Example.FunPtr.barA
    , Example.FunPtr.barB
    , Example.FunPtr.barC
    , Example.FunPtr.barD
    , Example.FunPtr.barE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/function.h>"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e113ab1229902001 (void)) ("
  , "  MyFunction *arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_5e83648377d8afc6 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d6d79c737e65f7e7 (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_566c44a793d222a9 (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9214c66c2650a721 (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_08a6ea4ce7e2c244 (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &fooE;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_15c0f27a0b5f01ca (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_barA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1e518c0403f5c339 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &barA;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_barB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6a1e56bd4a228268 (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &barB;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_barC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4edd36805e7f06a0 (void)) ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  return &barC;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_barD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1f340970ba2d8ec0 (void)) ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  return &barD;"
  , "}"
  , "/* test_bindingspecsfun_argmacrofu_Example_get_barE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1dd905a06024a69 (void)) ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  return &barE;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_e113ab1229902001" hs_bindgen_e113ab1229902001_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
hs_bindgen_e113ab1229902001 :: IO (BG.FunPtr (BG.FunPtr MyFunction -> IO ()))
hs_bindgen_e113ab1229902001 =
  BG.fromFFIType hs_bindgen_e113ab1229902001_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 6:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo :: BG.FunPtr (BG.FunPtr MyFunction -> IO ())
foo = BG.unsafePerformIO hs_bindgen_e113ab1229902001

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_5e83648377d8afc6" hs_bindgen_5e83648377d8afc6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
hs_bindgen_5e83648377d8afc6 :: IO (BG.FunPtr (BG.FunPtr A -> IO ()))
hs_bindgen_5e83648377d8afc6 =
  BG.fromFFIType hs_bindgen_5e83648377d8afc6_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 12:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA :: BG.FunPtr (BG.FunPtr A -> IO ())
fooA = BG.unsafePerformIO hs_bindgen_5e83648377d8afc6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d6d79c737e65f7e7" hs_bindgen_d6d79c737e65f7e7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
hs_bindgen_d6d79c737e65f7e7 :: IO (BG.FunPtr (BG.FunPtr B -> IO ()))
hs_bindgen_d6d79c737e65f7e7 =
  BG.fromFFIType hs_bindgen_d6d79c737e65f7e7_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB :: BG.FunPtr (BG.FunPtr B -> IO ())
fooB = BG.unsafePerformIO hs_bindgen_d6d79c737e65f7e7

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_566c44a793d222a9" hs_bindgen_566c44a793d222a9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooC@
hs_bindgen_566c44a793d222a9 :: IO (BG.FunPtr (BG.FunPtr M.C -> IO ()))
hs_bindgen_566c44a793d222a9 =
  BG.fromFFIType hs_bindgen_566c44a793d222a9_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooC :: BG.FunPtr (BG.FunPtr M.C -> IO ())
fooC = BG.unsafePerformIO hs_bindgen_566c44a793d222a9

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_9214c66c2650a721" hs_bindgen_9214c66c2650a721_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooD@
hs_bindgen_9214c66c2650a721 :: IO (BG.FunPtr (BG.FunPtr M.D -> IO ()))
hs_bindgen_9214c66c2650a721 =
  BG.fromFFIType hs_bindgen_9214c66c2650a721_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooD :: BG.FunPtr (BG.FunPtr M.D -> IO ())
fooD = BG.unsafePerformIO hs_bindgen_9214c66c2650a721

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_08a6ea4ce7e2c244" hs_bindgen_08a6ea4ce7e2c244_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooE@
hs_bindgen_08a6ea4ce7e2c244 :: IO (BG.FunPtr (BG.FunPtr E -> IO ()))
hs_bindgen_08a6ea4ce7e2c244 =
  BG.fromFFIType hs_bindgen_08a6ea4ce7e2c244_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooE :: BG.FunPtr (BG.FunPtr E -> IO ())
fooE = BG.unsafePerformIO hs_bindgen_08a6ea4ce7e2c244

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_15c0f27a0b5f01ca" hs_bindgen_15c0f27a0b5f01ca_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
hs_bindgen_15c0f27a0b5f01ca :: IO (BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ()))
hs_bindgen_15c0f27a0b5f01ca =
  BG.fromFFIType hs_bindgen_15c0f27a0b5f01ca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 40:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar :: BG.FunPtr (BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO ())
bar = BG.unsafePerformIO hs_bindgen_15c0f27a0b5f01ca

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_1e518c0403f5c339" hs_bindgen_1e518c0403f5c339_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
hs_bindgen_1e518c0403f5c339 :: IO (BG.FunPtr (BG.FunPtr A -> IO ()))
hs_bindgen_1e518c0403f5c339 =
  BG.fromFFIType hs_bindgen_1e518c0403f5c339_base

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 42:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA :: BG.FunPtr (BG.FunPtr A -> IO ())
barA = BG.unsafePerformIO hs_bindgen_1e518c0403f5c339

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_6a1e56bd4a228268" hs_bindgen_6a1e56bd4a228268_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
hs_bindgen_6a1e56bd4a228268 :: IO (BG.FunPtr (BG.FunPtr B -> IO ()))
hs_bindgen_6a1e56bd4a228268 =
  BG.fromFFIType hs_bindgen_6a1e56bd4a228268_base

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 43:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB :: BG.FunPtr (BG.FunPtr B -> IO ())
barB = BG.unsafePerformIO hs_bindgen_6a1e56bd4a228268

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barC@
foreign import ccall unsafe "hs_bindgen_4edd36805e7f06a0" hs_bindgen_4edd36805e7f06a0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barC@
hs_bindgen_4edd36805e7f06a0 :: IO (BG.FunPtr (BG.FunPtr M.C -> IO ()))
hs_bindgen_4edd36805e7f06a0 =
  BG.fromFFIType hs_bindgen_4edd36805e7f06a0_base

{-# NOINLINE barC #-}
{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 45:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barC :: BG.FunPtr (BG.FunPtr M.C -> IO ())
barC = BG.unsafePerformIO hs_bindgen_4edd36805e7f06a0

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barD@
foreign import ccall unsafe "hs_bindgen_1f340970ba2d8ec0" hs_bindgen_1f340970ba2d8ec0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barD@
hs_bindgen_1f340970ba2d8ec0 :: IO (BG.FunPtr (BG.FunPtr M.D -> IO ()))
hs_bindgen_1f340970ba2d8ec0 =
  BG.fromFFIType hs_bindgen_1f340970ba2d8ec0_base

{-# NOINLINE barD #-}
{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 46:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barD :: BG.FunPtr (BG.FunPtr M.D -> IO ())
barD = BG.unsafePerformIO hs_bindgen_1f340970ba2d8ec0

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barE@
foreign import ccall unsafe "hs_bindgen_e1dd905a06024a69" hs_bindgen_e1dd905a06024a69_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barE@
hs_bindgen_e1dd905a06024a69 :: IO (BG.FunPtr (BG.FunPtr E -> IO ()))
hs_bindgen_e1dd905a06024a69 =
  BG.fromFFIType hs_bindgen_e1dd905a06024a69_base

{-# NOINLINE barE #-}
{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 47:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barE :: BG.FunPtr (BG.FunPtr E -> IO ())
barE = BG.unsafePerformIO hs_bindgen_e1dd905a06024a69
