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
  [ "#include <binding-specs/fun_arg/struct.h>"
  , "/* test_bindingspecsfun_argstruct_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c2e17b622d82efca (void)) ("
  , "  struct S arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_bar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7a755908156c76ea (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &bar;"
  , "}"
  , "/* test_bindingspecsfun_argstruct_Example_get_baz */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c1d7cf62c138eea3 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &baz;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_c2e17b622d82efca" hs_bindgen_c2e17b622d82efca ::
     IO (Ptr.FunPtr (S -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:4:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
foo :: Ptr.FunPtr (S -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c2e17b622d82efca

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_bar@
foreign import ccall unsafe "hs_bindgen_7a755908156c76ea" hs_bindgen_7a755908156c76ea ::
     IO (Ptr.FunPtr (M1.A -> IO ()))

{-# NOINLINE bar #-}
{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:5:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
bar :: Ptr.FunPtr (M1.A -> IO ())
bar =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7a755908156c76ea

-- __unique:__ @test_bindingspecsfun_argstruct_Example_get_baz@
foreign import ccall unsafe "hs_bindgen_c1d7cf62c138eea3" hs_bindgen_c1d7cf62c138eea3 ::
     IO (Ptr.FunPtr (M2.B -> IO ()))

{-# NOINLINE baz #-}
{-| __C declaration:__ @baz@

    __defined at:__ @binding-specs\/fun_arg\/struct.h:6:6@

    __exported by:__ @binding-specs\/fun_arg\/struct.h@
-}
baz :: Ptr.FunPtr (M2.B -> IO ())
baz =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c1d7cf62c138eea3
