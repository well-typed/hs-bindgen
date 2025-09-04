{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <tentative_definitions.h>\n/* get_i1_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_736e69defba46ab4 (void) { return &i1; } \n/* get_i2_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_210c547ae5abcc02 (void) { return &i2; } \n/* get_i3_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_tentative_definitions_d6bb66d7f7107274 (void) { return &i3; } \n")

{-| __C declaration:__ @i1@

    __defined at:__ @tentative_definitions.h:17:5@

    __exported by:__ @tentative_definitions.h@
-}
foreign import ccall unsafe "hs_bindgen_test_tentative_definitions_736e69defba46ab4" hs_bindgen_test_tentative_definitions_736e69defba46ab4
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i1_ptr #-}

i1_ptr :: F.Ptr FC.CInt
i1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_tentative_definitions_736e69defba46ab4

{-| __C declaration:__ @i2@

    __defined at:__ @tentative_definitions.h:21:12@

    __exported by:__ @tentative_definitions.h@
-}
foreign import ccall unsafe "hs_bindgen_test_tentative_definitions_210c547ae5abcc02" hs_bindgen_test_tentative_definitions_210c547ae5abcc02
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i2_ptr #-}

i2_ptr :: F.Ptr FC.CInt
i2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_tentative_definitions_210c547ae5abcc02

{-| __C declaration:__ @i3@

    __defined at:__ @tentative_definitions.h:30:5@

    __exported by:__ @tentative_definitions.h@
-}
foreign import ccall unsafe "hs_bindgen_test_tentative_definitions_d6bb66d7f7107274" hs_bindgen_test_tentative_definitions_d6bb66d7f7107274
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE i3_ptr #-}

i3_ptr :: F.Ptr FC.CInt
i3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_tentative_definitions_d6bb66d7f7107274
