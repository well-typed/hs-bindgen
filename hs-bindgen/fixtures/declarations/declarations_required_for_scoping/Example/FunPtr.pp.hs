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
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "/* get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d" hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.FunPtr (A -> IO ()))
    )

hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d ::
     IO (Ptr.FunPtr (A -> IO ()))
hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d_base

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f_ptr :: Ptr.FunPtr (A -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationsdeclarations_requ_c34fd33eedc1490d
