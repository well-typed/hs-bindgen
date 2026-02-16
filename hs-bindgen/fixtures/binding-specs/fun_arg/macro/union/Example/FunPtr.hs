{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
foreign import ccall unsafe "hs_bindgen_cda902505d180c3d" hs_bindgen_cda902505d180c3d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_foo@
hs_bindgen_cda902505d180c3d :: IO (RIP.FunPtr (MyUnion -> IO ()))
hs_bindgen_cda902505d180c3d =
  RIP.fromFFIType hs_bindgen_cda902505d180c3d_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
foo :: RIP.FunPtr (MyUnion -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_cda902505d180c3d

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_d2d25b201a07a90e" hs_bindgen_d2d25b201a07a90e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooA@
hs_bindgen_d2d25b201a07a90e :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_d2d25b201a07a90e =
  RIP.fromFFIType hs_bindgen_d2d25b201a07a90e_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_d2d25b201a07a90e

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_4f49b9aa9b0c125d" hs_bindgen_4f49b9aa9b0c125d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooB@
hs_bindgen_4f49b9aa9b0c125d :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_4f49b9aa9b0c125d =
  RIP.fromFFIType hs_bindgen_4f49b9aa9b0c125d_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_4f49b9aa9b0c125d
