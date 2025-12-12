{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <program-analysis/program_slicing_selection.h>"
  , "enum FileOperationStatus hs_bindgen_654858ed6a5db417 ("
  , "  FILE *arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return read_file_chunk(arg1, arg2, arg3);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_654858ed6a5db417" read_file_chunk_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> FC.CSize
  -> IO FC.CInt

{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h:21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@

    __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_read_file_chunk@
-}
read_file_chunk ::
     Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     -- ^ __C declaration:__ @file_ptr@
  -> Ptr.Ptr Void
     -- ^ __C declaration:__ @buffer@
  -> HsBindgen.Runtime.Prelude.CSize
     -- ^ __C declaration:__ @bytes_to_read@
  -> IO FileOperationStatus
read_file_chunk =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType read_file_chunk_base
