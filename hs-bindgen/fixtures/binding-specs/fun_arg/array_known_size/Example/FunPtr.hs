{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/array_known_size.h>"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_630054fbd94e8bcc (void)) ("
  , "  signed int arg1[3]"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ca368a609d8fd5a8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argarray_kn_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2a2b0ebe94b0644c (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_630054fbd94e8bcc" hs_bindgen_630054fbd94e8bcc ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:3:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
foo :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_630054fbd94e8bcc

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_ca368a609d8fd5a8" hs_bindgen_ca368a609d8fd5a8 ::
     IO (Ptr.FunPtr (M1.A -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
bar :: Ptr.FunPtr (M1.A -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ca368a609d8fd5a8

-- __unique:__ @test_bindingspecsfun_argarray_kn_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_2a2b0ebe94b0644c" hs_bindgen_2a2b0ebe94b0644c ::
     IO (Ptr.FunPtr (M2.B -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/array_known_size.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/array_known_size.h@
-}
baz :: Ptr.FunPtr (M2.B -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2a2b0ebe94b0644c
