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
  , "/* Example_get_normal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_0f7b53dc37da18d6 (void)) ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside arg3"
  , ")"
  , "{"
  , "  return &normal;"
  , "}"
  , "/* Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_b864c2909d300a7a (void)) ("
  , "  struct named_struct arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsdecls_in_signature_66763a4fad90fe22 (void)) ("
  , "  union named_union arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

{-| __unique:__ @Example_get_normal_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_0f7b53dc37da18d6" hs_bindgen_test_functionsdecls_in_signature_0f7b53dc37da18d6 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))

{-# NOINLINE normal_ptr #-}

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h:7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal_ptr :: Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ())
normal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_0f7b53dc37da18d6

{-| __unique:__ @Example_get_f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_b864c2909d300a7a" hs_bindgen_test_functionsdecls_in_signature_b864c2909d300a7a ::
     IO (Ptr.FunPtr (Named_struct -> IO ()))

{-# NOINLINE f1_ptr #-}

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h:17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1_ptr :: Ptr.FunPtr (Named_struct -> IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_b864c2909d300a7a

{-| __unique:__ @Example_get_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsdecls_in_signature_66763a4fad90fe22" hs_bindgen_test_functionsdecls_in_signature_66763a4fad90fe22 ::
     IO (Ptr.FunPtr (Named_union -> IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h:20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2_ptr :: Ptr.FunPtr (Named_union -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsdecls_in_signature_66763a4fad90fe22
