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
  , "/* test_functionsdecls_in_signature_Example_get_normal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_3f49e28bee3f8746 (void)) ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside arg3"
  , ")"
  , "{"
  , "  return &normal;"
  , "}"
  , "/* test_functionsdecls_in_signature_Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8de29d760cad0c00 (void)) ("
  , "  struct named_struct arg1"
  , ")"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_functionsdecls_in_signature_Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_7a2b955aeef7fcd9 (void)) ("
  , "  union named_union arg1"
  , ")"
  , "{"
  , "  return &f2;"
  , "}"
  ]))

{-| __unique:__ @test_functionsdecls_in_signature_Example_get_normal_ptr@
-}
foreign import ccall unsafe "hs_bindgen_3f49e28bee3f8746" hs_bindgen_3f49e28bee3f8746 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ()))

{-# NOINLINE normal_ptr #-}

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h:7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal_ptr :: Ptr.FunPtr ((Ptr.Ptr Opaque) -> (Ptr.Ptr Outside) -> Outside -> IO ())
normal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3f49e28bee3f8746

{-| __unique:__ @test_functionsdecls_in_signature_Example_get_f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_8de29d760cad0c00" hs_bindgen_8de29d760cad0c00 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8de29d760cad0c00

{-| __unique:__ @test_functionsdecls_in_signature_Example_get_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_7a2b955aeef7fcd9" hs_bindgen_7a2b955aeef7fcd9 ::
     IO (Ptr.FunPtr (Named_union -> IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h:20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2_ptr :: Ptr.FunPtr (Named_union -> IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7a2b955aeef7fcd9
