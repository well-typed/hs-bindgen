{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <bool_c23.h>\n/* get_b_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_bool_c23_401ecb7e80957164 (void) { return &b; } \n")

foreign import ccall unsafe "hs_bindgen_test_bool_c23_401ecb7e80957164" hs_bindgen_test_bool_c23_401ecb7e80957164
  :: IO (F.Ptr FC.CBool)

{-# NOINLINE b_ptr #-}

b_ptr :: F.Ptr FC.CBool
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_bool_c23_401ecb7e80957164
