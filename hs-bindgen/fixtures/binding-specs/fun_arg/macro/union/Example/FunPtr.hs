{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.foo
    , Example.FunPtr.fooA
    , Example.FunPtr.fooB
    , Example.FunPtr.fooC
    , Example.FunPtr.fooD
    , Example.FunPtr.fooE
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
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
  , "/* test_bindingspecsfun_argmacroun_Example_get_fooC */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_669f2d7e801db0d0 (void)) ("
  , "  C arg1"
  , ")"
  , "{"
  , "  return &fooC;"
  , "}"
  , "/* test_bindingspecsfun_argmacroun_Example_get_fooD */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6504f8cec55c35ff (void)) ("
  , "  D arg1"
  , ")"
  , "{"
  , "  return &fooD;"
  , "}"
  , "/* test_bindingspecsfun_argmacroun_Example_get_fooE */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ae8e5c504bcf0849 (void)) ("
  , "  E arg1"
  , ")"
  , "{"
  , "  return &fooE;"
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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 6:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 12:6@

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

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 13:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_4f49b9aa9b0c125d

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooC@
foreign import ccall unsafe "hs_bindgen_669f2d7e801db0d0" hs_bindgen_669f2d7e801db0d0_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooC@
hs_bindgen_669f2d7e801db0d0 :: IO (RIP.FunPtr (M.C -> IO ()))
hs_bindgen_669f2d7e801db0d0 =
  RIP.fromFFIType hs_bindgen_669f2d7e801db0d0_base

{-# NOINLINE fooC #-}
{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooC :: RIP.FunPtr (M.C -> IO ())
fooC =
  RIP.unsafePerformIO hs_bindgen_669f2d7e801db0d0

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooD@
foreign import ccall unsafe "hs_bindgen_6504f8cec55c35ff" hs_bindgen_6504f8cec55c35ff_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooD@
hs_bindgen_6504f8cec55c35ff :: IO (RIP.FunPtr (M.D -> IO ()))
hs_bindgen_6504f8cec55c35ff =
  RIP.fromFFIType hs_bindgen_6504f8cec55c35ff_base

{-# NOINLINE fooD #-}
{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooD :: RIP.FunPtr (M.D -> IO ())
fooD =
  RIP.unsafePerformIO hs_bindgen_6504f8cec55c35ff

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooE@
foreign import ccall unsafe "hs_bindgen_ae8e5c504bcf0849" hs_bindgen_ae8e5c504bcf0849_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacroun_Example_get_fooE@
hs_bindgen_ae8e5c504bcf0849 :: IO (RIP.FunPtr (E -> IO ()))
hs_bindgen_ae8e5c504bcf0849 =
  RIP.fromFFIType hs_bindgen_ae8e5c504bcf0849_base

{-# NOINLINE fooE #-}
{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/union.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/union.h@
-}
fooE :: RIP.FunPtr (E -> IO ())
fooE =
  RIP.unsafePerformIO hs_bindgen_ae8e5c504bcf0849
