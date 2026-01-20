{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_global_counter@
hs_bindgen_f9dede86496f59c9 :: IO (Ptr.Ptr FC.CInt)
hs_bindgen_f9dede86496f59c9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f9dede86496f59c9_base

{-# NOINLINE global_counter #-}
{-|

  > extern int global_counter

  Global counter variable

  This variable tracks the number of operations performed.

__C declaration:__ @global_counter@

__defined at:__ @documentation\/doxygen_docs.h 61:12@

__exported by:__ @documentation\/doxygen_docs.h@
-}
global_counter :: Ptr.Ptr FC.CInt
global_counter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f9dede86496f59c9

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
foreign import ccall unsafe "hs_bindgen_a294be08386c9257" hs_bindgen_a294be08386c9257_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_documentationdoxygen_docs_Example_get_version_string@
hs_bindgen_a294be08386c9257 :: IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar))
hs_bindgen_a294be08386c9257 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a294be08386c9257_base

{-# NOINLINE version_string #-}
{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @documentation\/doxygen_docs.h 67:20@

__exported by:__ @documentation\/doxygen_docs.h@
-}
version_string :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)
version_string =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a294be08386c9257
