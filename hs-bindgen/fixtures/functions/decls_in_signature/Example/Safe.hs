{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.normal
    )
  where

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
  , "  (normal)(arg1, arg2, *arg3);"
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
