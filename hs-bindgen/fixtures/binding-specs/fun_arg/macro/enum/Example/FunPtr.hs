{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/enum.h>"
  , "/* test_bindingspecsfun_argmacroen_Example_get_foo */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_098964a440956602 (void)) ("
  , "  enum MyEnum arg1"
  , ")"
  , "{"
  , "  return &foo;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooA */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_86b685b9d27ce50e (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &fooA;"
  , "}"
  , "/* test_bindingspecsfun_argmacroen_Example_get_fooB */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9383ae52414c2c19 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &fooB;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_foo@
foreign import ccall unsafe "hs_bindgen_098964a440956602" hs_bindgen_098964a440956602_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_foo@
hs_bindgen_098964a440956602 :: IO (Ptr.FunPtr (MyEnum -> IO ()))
hs_bindgen_098964a440956602 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_098964a440956602_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
foo :: Ptr.FunPtr (MyEnum -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_098964a440956602

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_86b685b9d27ce50e" hs_bindgen_86b685b9d27ce50e_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooA@
hs_bindgen_86b685b9d27ce50e :: IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_86b685b9d27ce50e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_86b685b9d27ce50e_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86b685b9d27ce50e

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_9383ae52414c2c19" hs_bindgen_9383ae52414c2c19_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacroen_Example_get_fooB@
hs_bindgen_9383ae52414c2c19 :: IO (Ptr.FunPtr (B -> IO ()))
hs_bindgen_9383ae52414c2c19 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9383ae52414c2c19_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/enum.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/enum.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9383ae52414c2c19
