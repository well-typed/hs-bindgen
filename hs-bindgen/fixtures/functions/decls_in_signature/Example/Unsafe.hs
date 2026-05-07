{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.normal
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/decls_in_signature.h>"
  , "void hs_bindgen_247ee31a29b7e5a8 ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside *arg3"
  , ")"
  , "{"
  , "  (normal)(arg1, arg2, *arg3);"
  , "}"
  ]))

-- __unique:__ @test_functionsdecls_in_signature_Example_Unsafe_normal@
foreign import ccall unsafe "hs_bindgen_247ee31a29b7e5a8" hs_bindgen_247ee31a29b7e5a8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_functionsdecls_in_signature_Example_Unsafe_normal@
hs_bindgen_247ee31a29b7e5a8 ::
     RIP.Ptr Opaque
  -> RIP.Ptr Outside
  -> RIP.Ptr Outside
  -> IO ()
hs_bindgen_247ee31a29b7e5a8 =
  RIP.fromFFIType hs_bindgen_247ee31a29b7e5a8_base

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
                              hs_bindgen_247ee31a29b7e5a8 ptr_to_opaque0 ptr_to_defined1 by_value3)
