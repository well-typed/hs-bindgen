{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
hs_bindgen_e113ab1229902001 :: IO (Ptr.FunPtr ((Ptr.FunPtr MyFunction) -> IO ()))
hs_bindgen_e113ab1229902001 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e113ab1229902001_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
foo :: Ptr.FunPtr ((Ptr.FunPtr MyFunction) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e113ab1229902001

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_5e83648377d8afc6" hs_bindgen_5e83648377d8afc6_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
hs_bindgen_5e83648377d8afc6 :: IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_5e83648377d8afc6 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_5e83648377d8afc6_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5e83648377d8afc6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d6d79c737e65f7e7" hs_bindgen_d6d79c737e65f7e7_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
hs_bindgen_d6d79c737e65f7e7 :: IO (Ptr.FunPtr (B -> IO ()))
hs_bindgen_d6d79c737e65f7e7 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_d6d79c737e65f7e7_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d6d79c737e65f7e7

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_15c0f27a0b5f01ca" hs_bindgen_15c0f27a0b5f01ca_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_bar@
hs_bindgen_15c0f27a0b5f01ca :: IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))
hs_bindgen_15c0f27a0b5f01ca =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_15c0f27a0b5f01ca_base

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
bar :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_15c0f27a0b5f01ca

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
foreign import ccall unsafe "hs_bindgen_1e518c0403f5c339" hs_bindgen_1e518c0403f5c339_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barA@
hs_bindgen_1e518c0403f5c339 :: IO (Ptr.FunPtr ((Ptr.FunPtr A) -> IO ()))
hs_bindgen_1e518c0403f5c339 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_1e518c0403f5c339_base

{-# NOINLINE barA #-}
{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 36:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barA :: Ptr.FunPtr ((Ptr.FunPtr A) -> IO ())
barA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1e518c0403f5c339

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
foreign import ccall unsafe "hs_bindgen_6a1e56bd4a228268" hs_bindgen_6a1e56bd4a228268_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_barB@
hs_bindgen_6a1e56bd4a228268 :: IO (Ptr.FunPtr ((Ptr.FunPtr B) -> IO ()))
hs_bindgen_6a1e56bd4a228268 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_6a1e56bd4a228268_base

{-# NOINLINE barB #-}
{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function.h@
-}
barB :: Ptr.FunPtr ((Ptr.FunPtr B) -> IO ())
barB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6a1e56bd4a228268
