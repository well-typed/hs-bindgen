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
  [ "#include <doxygen_docs.h>"
  , "/* get_global_counter_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_doxygen_docs_1a40d1e5fbd04660 (void)"
  , "{"
  , "  return &global_counter;"
  , "}"
  , "/* get_version_string_ptr */"
  , "__attribute__ ((const))"
  , "char const **hs_bindgen_test_doxygen_docs_0f1cef8c70bbdf2c (void)"
  , "{"
  , "  return &version_string;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_1a40d1e5fbd04660" hs_bindgen_test_doxygen_docs_1a40d1e5fbd04660 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE global_counter_ptr #-}

{-|

  > extern int global_counter

  Global counter variable

  This variable tracks the number of operations performed.

__C declaration:__ @global_counter@

__defined at:__ @doxygen_docs.h:61:12@

__exported by:__ @doxygen_docs.h@
-}
global_counter_ptr :: Ptr.Ptr FC.CInt
global_counter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_1a40d1e5fbd04660

foreign import ccall unsafe "hs_bindgen_test_doxygen_docs_0f1cef8c70bbdf2c" hs_bindgen_test_doxygen_docs_0f1cef8c70bbdf2c ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE version_string_ptr #-}

{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @doxygen_docs.h:67:20@

__exported by:__ @doxygen_docs.h@
-}
version_string_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
version_string_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_doxygen_docs_0f1cef8c70bbdf2c
