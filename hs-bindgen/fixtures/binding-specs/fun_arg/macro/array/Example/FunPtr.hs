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
  [ "#include <binding-specs/fun_arg/macro/array.h>"
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
foreign import ccall unsafe "hs_bindgen_fc9d87cbca4127e4" hs_bindgen_fc9d87cbca4127e4 ::
     IO (Ptr.FunPtr (MyArray -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
foo :: Ptr.FunPtr (MyArray -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fc9d87cbca4127e4

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_0dbc306c53c94679" hs_bindgen_0dbc306c53c94679 ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0dbc306c53c94679

-- __unique:__ @test_bindingspecsfun_argmacroar_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_db0d885ae35d447f" hs_bindgen_db0d885ae35d447f ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/array.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/array.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_db0d885ae35d447f
