{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.read_file_chunk
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <program-analysis/program_slicing_selection.h>"
  , "enum FileOperationStatus hs_bindgen_b2a91b3b7edf2ad3 ("
  , "  FILE *arg1,"
  , "  void *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (read_file_chunk)(arg1, arg2, arg3);"
  , "}"
  ]))

-- __unique:__ @test_programanalysisprogram_slici_Example_Safe_read_file_chunk@
foreign import ccall safe "hs_bindgen_b2a91b3b7edf2ad3" hs_bindgen_b2a91b3b7edf2ad3_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Word64
  -> IO BG.Int32

-- __unique:__ @test_programanalysisprogram_slici_Example_Safe_read_file_chunk@
hs_bindgen_b2a91b3b7edf2ad3 ::
     BG.Ptr HsBindgen.Runtime.LibC.CFile
  -> BG.Ptr BG.Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO FileOperationStatus
hs_bindgen_b2a91b3b7edf2ad3 =
  BG.fromFFIType hs_bindgen_b2a91b3b7edf2ad3_base

{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h 21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk ::
     BG.Ptr HsBindgen.Runtime.LibC.CFile
     -- ^ __C declaration:__ @file_ptr@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @buffer@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @bytes_to_read@
  -> IO FileOperationStatus
read_file_chunk = hs_bindgen_b2a91b3b7edf2ad3
