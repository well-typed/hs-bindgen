{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_e113ab1229902001" hs_bindgen_e113ab1229902001_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
hs_bindgen_e113ab1229902001 :: IO (RIP.FunPtr ((RIP.FunPtr MyFunction) -> IO ()))
hs_bindgen_e113ab1229902001 =
  RIP.fromFFIType hs_bindgen_e113ab1229902001_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo :: RIP.FunPtr ((RIP.FunPtr MyFunction) -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_e113ab1229902001

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_5e83648377d8afc6" hs_bindgen_5e83648377d8afc6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
hs_bindgen_5e83648377d8afc6 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_5e83648377d8afc6 =
  RIP.fromFFIType hs_bindgen_5e83648377d8afc6_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_5e83648377d8afc6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d6d79c737e65f7e7" hs_bindgen_d6d79c737e65f7e7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
hs_bindgen_d6d79c737e65f7e7 :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_d6d79c737e65f7e7 =
  RIP.fromFFIType hs_bindgen_d6d79c737e65f7e7_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_d6d79c737e65f7e7

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_15c0f27a0b5f01ca" hs_bindgen_15c0f27a0b5f01ca_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
hs_bindgen_15c0f27a0b5f01ca :: IO (RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO ()))
hs_bindgen_15c0f27a0b5f01ca =
  RIP.fromFFIType hs_bindgen_15c0f27a0b5f01ca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar :: RIP.FunPtr ((RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO ())
bar = RIP.unsafePerformIO hs_bindgen_15c0f27a0b5f01ca

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_1e518c0403f5c339" hs_bindgen_1e518c0403f5c339_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
hs_bindgen_1e518c0403f5c339 :: IO (RIP.FunPtr ((RIP.FunPtr A) -> IO ()))
hs_bindgen_1e518c0403f5c339 =
  RIP.fromFFIType hs_bindgen_1e518c0403f5c339_base

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA :: RIP.FunPtr ((RIP.FunPtr A) -> IO ())
barA =
  RIP.unsafePerformIO hs_bindgen_1e518c0403f5c339

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_6a1e56bd4a228268" hs_bindgen_6a1e56bd4a228268_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
hs_bindgen_6a1e56bd4a228268 :: IO (RIP.FunPtr ((RIP.FunPtr B) -> IO ()))
hs_bindgen_6a1e56bd4a228268 =
  RIP.fromFFIType hs_bindgen_6a1e56bd4a228268_base

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB :: RIP.FunPtr ((RIP.FunPtr B) -> IO ())
barB =
  RIP.unsafePerformIO hs_bindgen_6a1e56bd4a228268
