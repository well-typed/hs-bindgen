{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource "#include <bool_c23.h>\n/* get_b_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_bool_c23_fcd0c984d664f6ee (void) { return &b; } \n")

{-| __C declaration:__ @b@

    __defined at:__ @bool_c23.h:3:13@

    __exported by:__ @bool_c23.h@
-}
foreign import ccall unsafe "hs_bindgen_test_bool_c23_fcd0c984d664f6ee" hs_bindgen_test_bool_c23_fcd0c984d664f6ee
  :: IO (Ptr.Ptr FC.CBool)

{-# NOINLINE b_ptr #-}

b_ptr :: Ptr.Ptr FC.CBool
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_bool_c23_fcd0c984d664f6ee
