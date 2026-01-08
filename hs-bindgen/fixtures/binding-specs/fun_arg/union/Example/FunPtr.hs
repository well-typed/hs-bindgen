{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M1
import qualified M2
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/union.h>"
  , "/* test_bindingspecsfun_argunion_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2ae32a336baf7eeb (void)) ("
  , "  union U arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f0f75b7b3e6fd8f8 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argunion_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0a962c2aa698b52f (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_2ae32a336baf7eeb" hs_bindgen_2ae32a336baf7eeb ::
     IO (Ptr.FunPtr (U -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/union.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
foo :: Ptr.FunPtr (U -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2ae32a336baf7eeb

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_f0f75b7b3e6fd8f8" hs_bindgen_f0f75b7b3e6fd8f8 ::
     IO (Ptr.FunPtr (M1.A -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/union.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
bar :: Ptr.FunPtr (M1.A -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f0f75b7b3e6fd8f8

-- __unique:__ @test_bindingspecsfun_argunion_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_0a962c2aa698b52f" hs_bindgen_0a962c2aa698b52f ::
     IO (Ptr.FunPtr (M2.B -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/union.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/union.h@
-}
baz :: Ptr.FunPtr (M2.B -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0a962c2aa698b52f
