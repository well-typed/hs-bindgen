{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <program-analysis/program_slicing_selection.h>"
  , "/* get_read_file_chunk_ptr */"
  , "__attribute__ ((const))"
  , "enum FileOperationStatus (*hs_bindgen_test_programanalysisprogram_slici_cc45351e6b02b3b4 (void)) ("
  , "  FILE *arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &read_file_chunk;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_read_file_chunk_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_programanalysisprogram_slici_cc45351e6b02b3b4" hs_bindgen_test_programanalysisprogram_slici_cc45351e6b02b3b4 ::
     IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FileOperationStatus))

{-# NOINLINE read_file_chunk_ptr #-}

{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h:21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk_ptr :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FileOperationStatus)
read_file_chunk_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_programanalysisprogram_slici_cc45351e6b02b3b4
