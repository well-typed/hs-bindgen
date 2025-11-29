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
  [ "#include <documentation/doxygen_docs.h>"
  , "/* Example_get_global_counter_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_documentationdoxygen_docs_b64b5b9b22522112 (void)"
  , "{"
  , "  return &global_counter;"
  , "}"
  , "/* Example_get_version_string_ptr */"
  , "__attribute__ ((const))"
  , "char const **hs_bindgen_test_documentationdoxygen_docs_b69007acfe74b444 (void)"
  , "{"
  , "  return &version_string;"
  , "}"
  ]))

{-| __unique:__ @Example_get_global_counter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_b64b5b9b22522112" hs_bindgen_test_documentationdoxygen_docs_b64b5b9b22522112 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE global_counter_ptr #-}

{-|

  > extern int global_counter

  Global counter variable

  This variable tracks the number of operations performed.

__C declaration:__ @global_counter@

__defined at:__ @documentation\/doxygen_docs.h:61:12@

__exported by:__ @documentation\/doxygen_docs.h@
-}
global_counter_ptr :: Ptr.Ptr FC.CInt
global_counter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_b64b5b9b22522112

{-| __unique:__ @Example_get_version_string_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_documentationdoxygen_docs_b69007acfe74b444" hs_bindgen_test_documentationdoxygen_docs_b69007acfe74b444 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE version_string_ptr #-}

{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @documentation\/doxygen_docs.h:67:20@

__exported by:__ @documentation\/doxygen_docs.h@
-}
version_string_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
version_string_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_documentationdoxygen_docs_b69007acfe74b444
