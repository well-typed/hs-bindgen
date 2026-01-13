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
  [ "#include <binding-specs/fun_arg/macro/union.h>"
  , "/* test_bindingspecsfun_argmacroun_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cda902505d180c3d (void)) ("
  , "  union MyUnion arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacroun_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d2d25b201a07a90e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacroun_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4f49b9aa9b0c125d (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_cda902505d180c3d" hs_bindgen_cda902505d180c3d ::
     IO (Ptr.FunPtr (MyUnion -> IO ()))

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
foo :: Ptr.FunPtr (MyUnion -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cda902505d180c3d

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_d2d25b201a07a90e" hs_bindgen_d2d25b201a07a90e ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d2d25b201a07a90e

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_4f49b9aa9b0c125d" hs_bindgen_4f49b9aa9b0c125d ::
     IO (Ptr.FunPtr (B -> IO ()))

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f49b9aa9b0c125d
