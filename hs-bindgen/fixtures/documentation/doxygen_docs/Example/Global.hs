{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <documentation/doxygen_docs.h>"
  , "/* test_documentationdoxygen_docs_Example_get_global_counter */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_f9dede86496f59c9 (void)"
  , "{"
  , "  return &global_counter;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_version_string */"
  , "__attribute__ ((const))"
  , "char const **hs_bindgen_a294be08386c9257 (void)"
  , "{"
  , "  return &version_string;"
  , "}"
  ]))

-- __unique:__ @test_documentationdoxygen_docs_Example_get_global_counter@
foreign import ccall unsafe "hs_bindgen_f9dede86496f59c9" hs_bindgen_f9dede86496f59c9_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_global_counter@
hs_bindgen_f9dede86496f59c9 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_f9dede86496f59c9 =
  RIP.fromFFIType hs_bindgen_f9dede86496f59c9_base

{-# NOINLINE global_counter #-}
{-|

  > extern int global_counter

  Global counter variable

  This variable tracks the number of operations performed.

__C declaration:__ @global_counter@

__defined at:__ @documentation\/doxygen_docs.h 61:12@

__exported by:__ @documentation\/doxygen_docs.h@
-}
global_counter :: RIP.Ptr RIP.CInt
global_counter =
  RIP.unsafePerformIO hs_bindgen_f9dede86496f59c9

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
foreign import ccall unsafe "hs_bindgen_a294be08386c9257" hs_bindgen_a294be08386c9257_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
hs_bindgen_a294be08386c9257 :: IO (RIP.Ptr (PtrConst.PtrConst RIP.CChar))
hs_bindgen_a294be08386c9257 =
  RIP.fromFFIType hs_bindgen_a294be08386c9257_base

{-# NOINLINE version_string #-}
{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @documentation\/doxygen_docs.h 67:20@

__exported by:__ @documentation\/doxygen_docs.h@
-}
version_string :: RIP.Ptr (PtrConst.PtrConst RIP.CChar)
version_string =
  RIP.unsafePerformIO hs_bindgen_a294be08386c9257
