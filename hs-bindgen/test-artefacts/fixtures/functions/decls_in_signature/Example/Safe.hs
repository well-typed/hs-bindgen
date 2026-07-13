{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.normal
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_functionsdecls_in_signature_Example_Safe_normal@
hs_bindgen_920e5c20f770432b ::
     BG.Ptr Opaque
  -> BG.Ptr Outside
  -> BG.Ptr Outside
  -> IO ()
hs_bindgen_920e5c20f770432b =
  BG.fromFFIType hs_bindgen_920e5c20f770432b_base

{-| __C declaration:__ @normal@

    __defined at:__ @functions\/decls_in_signature.h 7:6@

    __exported by:__ @functions\/decls_in_signature.h@
-}
normal ::
     BG.Ptr Opaque
     -- ^ __C declaration:__ @ptr_to_opaque@
  -> BG.Ptr Outside
     -- ^ __C declaration:__ @ptr_to_defined@
  -> Outside
     -- ^ __C declaration:__ @by_value@
  -> IO ()
normal =
  \ptr_to_opaque0 ->
    \ptr_to_defined1 ->
      \by_value2 ->
        BG.with by_value2 (\by_value3 ->
                             hs_bindgen_920e5c20f770432b ptr_to_opaque0 ptr_to_defined1 by_value3)
