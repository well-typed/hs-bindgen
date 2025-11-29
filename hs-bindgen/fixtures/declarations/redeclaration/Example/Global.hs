{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/redeclaration.h>"
  , "/* Example_get_x_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_declarationsredeclaration_8e2611056411c1b6 (void)"
  , "{"
  , "  return &x;"
  , "}"
  ]))

{-| __unique:__ @Example_get_x_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsredeclaration_8e2611056411c1b6" hs_bindgen_test_declarationsredeclaration_8e2611056411c1b6 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE x_ptr #-}

{-| __C declaration:__ @x@

    __defined at:__ @declarations\/redeclaration.h:11:5@

    __exported by:__ @declarations\/redeclaration.h@
-}
x_ptr :: Ptr.Ptr FC.CInt
x_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_declarationsredeclaration_8e2611056411c1b6
