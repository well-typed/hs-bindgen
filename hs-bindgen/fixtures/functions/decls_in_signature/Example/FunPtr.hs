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
  [ "#include <functions/decls_in_signature.h>"
  , "/* test_functionsdecls_in_signature_Example_get_normal */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f3036965ea57b87f (void)) ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside arg3"
  , ")"
  , "{"
  , "  return &normal;"
  , "}"
  , "/* test_functionsdecls_in_signature_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_86a0bd6e9f7eb005 (void)) ("
  , "  struct named_struct arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_functionsdecls_in_signature_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_4bb469a35be04698 (void)) ("
  , "  union named_union arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

-- __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
foreign import ccall unsafe "hs_bindgen_f3036965ea57b87f" hs_bindgen_f3036965ea57b87f_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
hs_bindgen_f3036965ea57b87f :: IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))
hs_bindgen_f3036965ea57b87f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f3036965ea57b87f_base

{-# NOINLINE normal_funptr #-}
{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h 7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal_funptr :: Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ())
normal_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3036965ea57b87f

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_86a0bd6e9f7eb005" hs_bindgen_86a0bd6e9f7eb005_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f1@
hs_bindgen_86a0bd6e9f7eb005 :: IO (Ptr.FunPtr (Named_struct -> IO ()))
hs_bindgen_86a0bd6e9f7eb005 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_86a0bd6e9f7eb005_base

{-# NOINLINE f1_funptr #-}
{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h 17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1_funptr :: Ptr.FunPtr (Named_struct -> IO ())
f1_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86a0bd6e9f7eb005

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_4bb469a35be04698" hs_bindgen_4bb469a35be04698_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f2@
hs_bindgen_4bb469a35be04698 :: IO (Ptr.FunPtr (Named_union -> IO ()))
hs_bindgen_4bb469a35be04698 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4bb469a35be04698_base

{-# NOINLINE f2_funptr #-}
{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h 20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2_funptr :: Ptr.FunPtr (Named_union -> IO ())
f2_funptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4bb469a35be04698
