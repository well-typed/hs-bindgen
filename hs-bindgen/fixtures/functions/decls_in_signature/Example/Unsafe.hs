{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/decls_in_signature.h>"
  , "void hs_bindgen_247ee31a29b7e5a8 ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside *arg3"
  , ")"
  , "{"
  , "  normal(arg1, arg2, *arg3);"
  , "}"
  , "void hs_bindgen_dad6e1aa83dec458 ("
  , "  struct named_struct *arg1"
  , ")"
  , "{"
  , "  f1(*arg1);"
  , "}"
  , "void hs_bindgen_e6bb0f3956383df9 ("
  , "  union named_union *arg1"
  , ")"
  , "{"
  , "  f2(*arg1);"
  , "}"
  ]))

-- | __unique:__ @test_functionsdecls_in_signature_Example_Unsafe_normal@
foreign import ccall unsafe "hs_bindgen_247ee31a29b7e5a8" hs_bindgen_247ee31a29b7e5a8 ::
     Ptr.Ptr Opaque
  -> Ptr.Ptr Outside
  -> Ptr.Ptr Outside
  -> IO ()

{-| Pointer-based API for 'normal'
-}
normal_wrapper ::
     Ptr.Ptr Opaque
     -- ^ __C declaration:__ @ptr_to_opaque@
  -> Ptr.Ptr Outside
     -- ^ __C declaration:__ @ptr_to_defined@
  -> Ptr.Ptr Outside
     -- ^ __C declaration:__ @by_value@
  -> IO ()
normal_wrapper = hs_bindgen_247ee31a29b7e5a8

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h:7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal ::
     Ptr.Ptr Opaque
     -- ^ __C declaration:__ @ptr_to_opaque@
  -> Ptr.Ptr Outside
     -- ^ __C declaration:__ @ptr_to_defined@
  -> Outside
     -- ^ __C declaration:__ @by_value@
  -> IO ()
normal =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x2 (\y3 ->
                     hs_bindgen_247ee31a29b7e5a8 x0 x1 y3)

-- | __unique:__ @test_functionsdecls_in_signature_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_dad6e1aa83dec458" hs_bindgen_dad6e1aa83dec458 ::
     Ptr.Ptr Named_struct
  -> IO ()

{-| Pointer-based API for 'f1'
-}
f1_wrapper ::
     Ptr.Ptr Named_struct
     -- ^ __C declaration:__ @arg@
  -> IO ()
f1_wrapper = hs_bindgen_dad6e1aa83dec458

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h:17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1 ::
     Named_struct
     -- ^ __C declaration:__ @arg@
  -> IO ()
f1 =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_dad6e1aa83dec458 y1)

-- | __unique:__ @test_functionsdecls_in_signature_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_e6bb0f3956383df9" hs_bindgen_e6bb0f3956383df9 ::
     Ptr.Ptr Named_union
  -> IO ()

{-| Pointer-based API for 'f2'
-}
f2_wrapper ::
     Ptr.Ptr Named_union
     -- ^ __C declaration:__ @arg@
  -> IO ()
f2_wrapper = hs_bindgen_e6bb0f3956383df9

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h:20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2 ::
     Named_union
     -- ^ __C declaration:__ @arg@
  -> IO ()
f2 =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_e6bb0f3956383df9 y1)
