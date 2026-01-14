{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_e113ab1229902001" hs_bindgen_e113ab1229902001 ::
     IO (Ptr.FunPtr (MyFunctionPointer -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
foo :: Ptr.FunPtr (MyFunctionPointer -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e113ab1229902001

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_5e83648377d8afc6" hs_bindgen_5e83648377d8afc6 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5e83648377d8afc6

-- __unique:__ @test_bindingspecsfun_argmacrofu_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_d6d79c737e65f7e7" hs_bindgen_d6d79c737e65f7e7 ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/function_pointer.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/function_pointer.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d6d79c737e65f7e7
