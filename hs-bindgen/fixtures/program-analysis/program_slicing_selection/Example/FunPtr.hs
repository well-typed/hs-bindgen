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
  , "/* test_programanalysisprogram_slici_Example_get_read_file_chunk */"
  , "__attribute__ ((const))"
  , "enum FileOperationStatus (*hs_bindgen_6e12e70d71890a10 (void)) ("
  , "  FILE *arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &read_file_chunk;"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_get_read_file_chunk@
foreign import ccall unsafe "hs_bindgen_6e12e70d71890a10" hs_bindgen_6e12e70d71890a10 ::
     IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FileOperationStatus))

{-# NOINLINE read_file_chunk #-}
{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h:21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.Prelude.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.CSize -> IO FileOperationStatus)
read_file_chunk =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6e12e70d71890a10
