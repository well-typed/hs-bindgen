{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/decls_in_signature.h>"
  , "/* get_normal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e (void)) ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside arg3"
  , ")"
  , "{"
  , "  return &normal;"
  , "}"
  , "/* get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1 (void)) ("
  , "  struct named_struct arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69 (void)) ("
  , "  union named_union arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e" hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))
    )

hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e ::
     IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))
hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e_base

{-# NOINLINE normal_ptr #-}

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h:7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal_ptr :: Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ())
normal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_b040d51578b7b05e

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1" hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (Named_struct -> IO ()))
    )

hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1 ::
     IO (Ptr.FunPtr (Named_struct -> IO ()))
hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1_base

{-# NOINLINE f1_ptr #-}

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h:17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1_ptr :: Ptr.FunPtr (Named_struct -> IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_5469bdc0395f86c1

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69" hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (Named_union -> IO ()))
    )

hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69 ::
     IO (Ptr.FunPtr (Named_union -> IO ()))
hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69_base

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h:20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2_ptr :: Ptr.FunPtr (Named_union -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_490ca7e8c8282a69
