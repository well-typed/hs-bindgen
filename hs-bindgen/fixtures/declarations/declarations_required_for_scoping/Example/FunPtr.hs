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
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "/* Example_get_f_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_declarationsdeclarations_requ_e684d4bc6355e13a (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

{-| __unique:__ @Example_get_f_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsdeclarations_requ_e684d4bc6355e13a" hs_bindgen_test_declarationsdeclarations_requ_e684d4bc6355e13a ::
     IO (Ptr.FunPtr (A -> IO ()))

{-# NOINLINE f_ptr #-}

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f_ptr :: Ptr.FunPtr (A -> IO ())
f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationsdeclarations_requ_e684d4bc6355e13a
