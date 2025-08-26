{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI

$(CAPI.addCSource "#include <tentative_definitions.h>\n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_d6bb66d7f7107274 (void) { return &i3; } \n")

foreign import ccall safe "hs_bindgen_test_tentative_definitions_736e69defba46ab4" i1_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_tentative_definitions_210c547ae5abcc02" i2_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_tentative_definitions_d6bb66d7f7107274" i3_ptr
  :: F.Ptr FC.CInt
