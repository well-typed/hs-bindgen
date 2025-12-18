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

-- | __unique:__ @test_functionsdecls_in_signature_Example_get_normal@
foreign import ccall unsafe "hs_bindgen_f3036965ea57b87f" hs_bindgen_f3036965ea57b87f ::
     IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))

{-# NOINLINE normal #-}

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h:7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal :: Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ())
normal =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f3036965ea57b87f

-- | __unique:__ @test_functionsdecls_in_signature_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_86a0bd6e9f7eb005" hs_bindgen_86a0bd6e9f7eb005 ::
     IO (Ptr.FunPtr (Named_struct -> IO ()))

{-# NOINLINE f1 #-}

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h:17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1 :: Ptr.FunPtr (Named_struct -> IO ())
f1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86a0bd6e9f7eb005

-- | __unique:__ @test_functionsdecls_in_signature_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_4bb469a35be04698" hs_bindgen_4bb469a35be04698 ::
     IO (Ptr.FunPtr (Named_union -> IO ()))

{-# NOINLINE f2 #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h:20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2 :: Ptr.FunPtr (Named_union -> IO ())
f2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4bb469a35be04698
