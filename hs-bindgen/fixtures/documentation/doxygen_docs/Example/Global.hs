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
  , "/* test_documentationdoxygen_docs_Example_get_global_counter_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a568b76e8feb0427 (void)"
  , "{"
  , "  return &global_counter;"
  , "}"
  , "/* test_documentationdoxygen_docs_Example_get_version_string_ptr */"
  , "__attribute__ ((const))"
  , "char const **hs_bindgen_dd671052fd43d189 (void)"
  , "{"
  , "  return &version_string;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a568b76e8feb0427" hs_bindgen_a568b76e8feb0427_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_documentationdoxygen_docs_Example_get_global_counter_ptr@
hs_bindgen_a568b76e8feb0427 ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_a568b76e8feb0427 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a568b76e8feb0427_base

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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a568b76e8feb0427

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dd671052fd43d189" hs_bindgen_dd671052fd43d189_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_documentationdoxygen_docs_Example_get_version_string_ptr@
hs_bindgen_dd671052fd43d189 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar))
hs_bindgen_dd671052fd43d189 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dd671052fd43d189_base

{-# NOINLINE version_string_ptr #-}

{-|

  > extern const char* version_string

  Version string constant

__C declaration:__ @version_string@

__defined at:__ @documentation\/doxygen_docs.h:67:20@

__exported by:__ @documentation\/doxygen_docs.h@
-}
version_string_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar)
version_string_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dd671052fd43d189
