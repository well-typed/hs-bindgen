{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <decls_in_signature.h>"
  , "void hs_bindgen_test_decls_in_signature_001a08d4459ec455 ("
  , "  struct opaque *arg1,"
  , "  struct outside *arg2,"
  , "  struct outside *arg3"
  , ")"
  , "{"
  , "  normal(arg1, arg2, *arg3);"
  , "}"
  , "void hs_bindgen_test_decls_in_signature_a2f84d2570ef3892 ("
  , "  struct named_struct *arg1"
  , ")"
  , "{"
  , "  f1(*arg1);"
  , "}"
  , "void hs_bindgen_test_decls_in_signature_1d043de05a457e90 ("
  , "  union named_union *arg1"
  , ")"
  , "{"
  , "  f2(*arg1);"
  , "}"
  ]))

{-| Pointer-based API for 'normal'

-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_001a08d4459ec455" normal_wrapper ::
     Ptr.Ptr Opaque
  -> Ptr.Ptr Outside
  -> Ptr.Ptr Outside
  -> IO ()

{-| __C declaration:__ @normal@

    __defined at:__ @decls_in_signature.h:7:6@

    __exported by:__ @decls_in_signature.h@
-}
normal ::
     Ptr.Ptr Opaque
     {- ^ __C declaration:__ @ptr_to_opaque@
     -}
  -> Ptr.Ptr Outside
     {- ^ __C declaration:__ @ptr_to_defined@
     -}
  -> Outside
     {- ^ __C declaration:__ @by_value@
     -}
  -> IO ()
normal =
  \x0 ->
    \x1 ->
      \x2 -> F.with x2 (\y3 -> normal_wrapper x0 x1 y3)

{-| Pointer-based API for 'f1'

-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_a2f84d2570ef3892" f1_wrapper ::
     Ptr.Ptr Named_struct
  -> IO ()

{-| Error cases

  See 'UnexpectedAnonInSignature' for discussion (of both these error cases and the edge cases below).

__C declaration:__ @f1@

__defined at:__ @decls_in_signature.h:17:6@

__exported by:__ @decls_in_signature.h@
-}
f1 ::
     Named_struct
     {- ^ __C declaration:__ @arg@
     -}
  -> IO ()
f1 = \x0 -> F.with x0 (\y1 -> f1_wrapper y1)

{-| Pointer-based API for 'f2'

-}
foreign import ccall safe "hs_bindgen_test_decls_in_signature_1d043de05a457e90" f2_wrapper ::
     Ptr.Ptr Named_union
  -> IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @decls_in_signature.h:20:6@

    __exported by:__ @decls_in_signature.h@
-}
f2 ::
     Named_union
     {- ^ __C declaration:__ @arg@
     -}
  -> IO ()
f2 = \x0 -> F.with x0 (\y1 -> f2_wrapper y1)
