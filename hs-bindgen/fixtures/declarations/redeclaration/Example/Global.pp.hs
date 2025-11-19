{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/redeclaration.h>"
  , "/* get_x_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationsredeclaration_10b125673bf2041b (void)"
  , "{"
  , "  return &x;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsredeclaration_10b125673bf2041b" hs_bindgen_test_declarationsredeclaration_10b125673bf2041b_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr FC.CInt)
    )

hs_bindgen_test_declarationsredeclaration_10b125673bf2041b ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_test_declarationsredeclaration_10b125673bf2041b =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType hs_bindgen_test_declarationsredeclaration_10b125673bf2041b_base

{-# NOINLINE x_ptr #-}

{-| __C declaration:__ @x@

    __defined at:__ @declarations\/redeclaration.h:11:5@

    __exported by:__ @declarations\/redeclaration.h@
-}
x_ptr :: Ptr.Ptr FC.CInt
x_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationsredeclaration_10b125673bf2041b
