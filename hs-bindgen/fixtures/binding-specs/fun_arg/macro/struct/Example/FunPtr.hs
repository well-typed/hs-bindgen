{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_foo@
hs_bindgen_ccfc23165c7fd4a9 :: IO (Ptr.FunPtr (MyStruct -> IO ()))
hs_bindgen_ccfc23165c7fd4a9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ccfc23165c7fd4a9_base

{-# NOINLINE foo #-}
{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo :: Ptr.FunPtr (MyStruct -> IO ())
foo =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ccfc23165c7fd4a9

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
foreign import ccall unsafe "hs_bindgen_ab74a4a30349b6b2" hs_bindgen_ab74a4a30349b6b2_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooA@
hs_bindgen_ab74a4a30349b6b2 :: IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_ab74a4a30349b6b2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ab74a4a30349b6b2_base

{-# NOINLINE fooA #-}
{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA :: Ptr.FunPtr (A -> IO ())
fooA =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ab74a4a30349b6b2

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
foreign import ccall unsafe "hs_bindgen_19855bed49223360" hs_bindgen_19855bed49223360_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_get_fooB@
hs_bindgen_19855bed49223360 :: IO (Ptr.FunPtr (B -> IO ()))
hs_bindgen_19855bed49223360 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_19855bed49223360_base

{-# NOINLINE fooB #-}
{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB :: Ptr.FunPtr (B -> IO ())
fooB =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_19855bed49223360
