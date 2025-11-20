{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <program-analysis/program_slicing_selection.h>"
  , "enum FileOperationStatus hs_bindgen_test_programanalysisprogram_slici_13b0ed81415a625a ("
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
foreign import ccall safe "hs_bindgen_test_programanalysisprogram_slici_13b0ed81415a625a" read_file_chunk_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
    -> Ptr.Ptr Void
    -> HsBindgen.Runtime.Prelude.CSize
    -> IO FileOperationStatus
    )

{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h:21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk ::
     Ptr.Ptr HsBindgen.Runtime.Prelude.CFile
     {- ^ __C declaration:__ @file_ptr@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @buffer@
     -}
  -> HsBindgen.Runtime.Prelude.CSize
     {- ^ __C declaration:__ @bytes_to_read@
     -}
  -> IO FileOperationStatus
read_file_chunk =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType read_file_chunk_base
