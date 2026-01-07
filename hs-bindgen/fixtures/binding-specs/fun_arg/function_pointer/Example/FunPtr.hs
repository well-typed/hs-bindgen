{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/function_pointer.h>"
  , "/* test_bindingspecsfun_argfunction_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8a0babf894a65b5f (void)) ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bb54c0055348a008 (void)) ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argfunction_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c69c3ad6f41d95ac (void)) ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_8a0babf894a65b5f" hs_bindgen_8a0babf894a65b5f ::
     IO (Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
foo :: Ptr.FunPtr ((Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8a0babf894a65b5f

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_bb54c0055348a008" hs_bindgen_bb54c0055348a008 ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M1.A) -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
bar :: Ptr.FunPtr ((Ptr.FunPtr M1.A) -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bb54c0055348a008

-- __unique:__ @test_bindingspecsfun_argfunction_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_c69c3ad6f41d95ac" hs_bindgen_c69c3ad6f41d95ac ::
     IO (Ptr.FunPtr ((Ptr.FunPtr M2.B) -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/function_pointer.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/function_pointer.h@
-}
baz :: Ptr.FunPtr ((Ptr.FunPtr M2.B) -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c69c3ad6f41d95ac
