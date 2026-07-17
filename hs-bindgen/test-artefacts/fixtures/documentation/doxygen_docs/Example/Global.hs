{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( -- * Core Data Types
      Example.Global.global_counter
    , Example.Global.version_string
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_global_counter@
hs_bindgen_f9dede86496f59c9 :: IO (BG.Ptr BG.CInt)
hs_bindgen_f9dede86496f59c9 =
  BG.fromFFIType hs_bindgen_f9dede86496f59c9_base

{-# NOINLINE global_counter #-}
{-| Global counter variable.

    This variable tracks the number of operations performed.

    __C declaration:__ @global_counter@

    __defined at:__ @documentation\/doxygen_docs.h 63:12@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
global_counter :: BG.Ptr BG.CInt
global_counter =
  BG.unsafePerformIO hs_bindgen_f9dede86496f59c9

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
foreign import ccall unsafe "hs_bindgen_a294be08386c9257" hs_bindgen_a294be08386c9257_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
hs_bindgen_a294be08386c9257 :: IO (BG.Ptr (PtrConst.PtrConst BG.CChar))
hs_bindgen_a294be08386c9257 =
  BG.fromFFIType hs_bindgen_a294be08386c9257_base

{-# NOINLINE version_string #-}
{-| Version string constant.

    __C declaration:__ @version_string@

    __defined at:__ @documentation\/doxygen_docs.h 69:20@

    __exported by:__ @documentation\/doxygen_docs.h@
-}
version_string :: BG.Ptr (PtrConst.PtrConst BG.CChar)
version_string =
  BG.unsafePerformIO hs_bindgen_a294be08386c9257
