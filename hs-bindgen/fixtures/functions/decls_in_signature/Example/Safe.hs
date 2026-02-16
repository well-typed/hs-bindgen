{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/decls_in_signature.h>"
  , "void hs_bindgen_920e5c20f770432b ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside *arg3"
  , ")"
  , "{"
  , "  normal(arg1, arg2, *arg3);"
  , "}"
  , "void hs_bindgen_baea2c7a0c8b9965 ("
  , "  struct named_struct *arg1"
  , ")"
  , "{"
  , "  f1(*arg1);"
  , "}"
  , "void hs_bindgen_990d7be722ad5414 ("
  , "  union named_union *arg1"
  , ")"
  , "{"
  , "  f2(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_normal@
foreign import ccall safe "hs_bindgen_920e5c20f770432b" hs_bindgen_920e5c20f770432b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_normal@
hs_bindgen_920e5c20f770432b ::
     RIP.Ptr Opaque
  -> RIP.Ptr Outside
  -> RIP.Ptr Outside
  -> IO ()
hs_bindgen_920e5c20f770432b =
  RIP.fromFFIType hs_bindgen_920e5c20f770432b_base

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h 7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal ::
     RIP.Ptr Opaque
     -- ^ __C declaration:__ @ptr_to_opaque@
  -> RIP.Ptr Outside
     -- ^ __C declaration:__ @ptr_to_defined@
  -> Outside
     -- ^ __C declaration:__ @by_value@
  -> IO ()
normal =
  \ptr_to_opaque0 ->
    \ptr_to_defined1 ->
      \by_value2 ->
        RIP.with by_value2 (\by_value3 ->
                              hs_bindgen_920e5c20f770432b ptr_to_opaque0 ptr_to_defined1 by_value3)

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_baea2c7a0c8b9965" hs_bindgen_baea2c7a0c8b9965_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_f1@
hs_bindgen_baea2c7a0c8b9965 ::
     RIP.Ptr Named_struct
  -> IO ()
hs_bindgen_baea2c7a0c8b9965 =
  RIP.fromFFIType hs_bindgen_baea2c7a0c8b9965_base

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @functions\/decls_in_signature.h 17:6@

__exported by:__ @functions\/decls_in_signature.h@
-}
f1 ::
     Named_struct
     -- ^ __C declaration:__ @arg@
  -> IO ()
f1 =
  \arg0 ->
    RIP.with arg0 (\arg1 ->
                     hs_bindgen_baea2c7a0c8b9965 arg1)

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_990d7be722ad5414" hs_bindgen_990d7be722ad5414_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_f2@
hs_bindgen_990d7be722ad5414 ::
     RIP.Ptr Named_union
  -> IO ()
hs_bindgen_990d7be722ad5414 =
  RIP.fromFFIType hs_bindgen_990d7be722ad5414_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/decls_in_signature.h 20:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
f2 ::
     Named_union
     -- ^ __C declaration:__ @arg@
  -> IO ()
f2 =
  \arg0 ->
    RIP.with arg0 (\arg1 ->
                     hs_bindgen_990d7be722ad5414 arg1)
