{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "/* test_bindingspecsfun_argmacrost_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ccfc23165c7fd4a9 (void)) ("
  , "  struct MyStruct arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ab74a4a30349b6b2 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacrost_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_19855bed49223360 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_ccfc23165c7fd4a9" hs_bindgen_ccfc23165c7fd4a9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_foo@
hs_bindgen_ccfc23165c7fd4a9 :: IO (RIP.FunPtr (MyStruct -> IO ()))
hs_bindgen_ccfc23165c7fd4a9 =
  RIP.fromFFIType hs_bindgen_ccfc23165c7fd4a9_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo :: RIP.FunPtr (MyStruct -> IO ())
foo = RIP.unsafePerformIO hs_bindgen_ccfc23165c7fd4a9

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_ab74a4a30349b6b2" hs_bindgen_ab74a4a30349b6b2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
hs_bindgen_ab74a4a30349b6b2 :: IO (RIP.FunPtr (A -> IO ()))
hs_bindgen_ab74a4a30349b6b2 =
  RIP.fromFFIType hs_bindgen_ab74a4a30349b6b2_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA :: RIP.FunPtr (A -> IO ())
fooA =
  RIP.unsafePerformIO hs_bindgen_ab74a4a30349b6b2

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_19855bed49223360" hs_bindgen_19855bed49223360_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
hs_bindgen_19855bed49223360 :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_19855bed49223360 =
  RIP.fromFFIType hs_bindgen_19855bed49223360_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB :: RIP.FunPtr (B -> IO ())
fooB =
  RIP.unsafePerformIO hs_bindgen_19855bed49223360
