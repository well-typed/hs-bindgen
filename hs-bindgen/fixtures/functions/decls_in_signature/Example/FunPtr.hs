{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
hs_bindgen_f3036965ea57b87f :: IO (RIP.FunPtr ((RIP.Ptr Opaque) -> (RIP.Ptr Outside) -> Outside -> IO ()))
hs_bindgen_f3036965ea57b87f =
  RIP.fromFFIType hs_bindgen_f3036965ea57b87f_base

{-# NOINLINE normal #-}
{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h 7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal :: RIP.FunPtr ((RIP.Ptr Opaque) -> (RIP.Ptr Outside) -> Outside -> IO ())
normal =
  RIP.unsafePerformIO hs_bindgen_f3036965ea57b87f

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_86a0bd6e9f7eb005" hs_bindgen_86a0bd6e9f7eb005_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f1@
hs_bindgen_86a0bd6e9f7eb005 :: IO (RIP.FunPtr (Named_struct -> IO ()))
hs_bindgen_86a0bd6e9f7eb005 =
  RIP.fromFFIType hs_bindgen_86a0bd6e9f7eb005_base

{-# NOINLINE f1 #-}
{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h 17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1 :: RIP.FunPtr (Named_struct -> IO ())
f1 = RIP.unsafePerformIO hs_bindgen_86a0bd6e9f7eb005

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_4bb469a35be04698" hs_bindgen_4bb469a35be04698_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsdecls_in_signature_Example_get_f2@
hs_bindgen_4bb469a35be04698 :: IO (RIP.FunPtr (Named_union -> IO ()))
hs_bindgen_4bb469a35be04698 =
  RIP.fromFFIType hs_bindgen_4bb469a35be04698_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h 20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2 :: RIP.FunPtr (Named_union -> IO ())
f2 = RIP.unsafePerformIO hs_bindgen_4bb469a35be04698
