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
  [ "#include <redeclaration.h>"
  , "/* get_x_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_redeclaration_10b125673bf2041b (void)"
  , "{"
  , "  return &x;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_redeclaration_10b125673bf2041b" hs_bindgen_test_redeclaration_10b125673bf2041b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE x_ptr #-}

{-| __C declaration:__ @x@

    __defined at:__ @redeclaration.h:11:5@

    __exported by:__ @redeclaration.h@
-}
x_ptr :: Ptr.Ptr FC.CInt
x_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_redeclaration_10b125673bf2041b
