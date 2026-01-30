{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_read_file_chunk@
foreign import ccall unsafe "hs_bindgen_654858ed6a5db417" hs_bindgen_654858ed6a5db417_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> GHC.Word.Word64
  -> IO GHC.Int.Int32

-- __unique:__ @test_programanalysisprogram_slici_Example_Unsafe_read_file_chunk@
hs_bindgen_654858ed6a5db417 ::
     Ptr.Ptr HsBindgen.Runtime.LibC.CFile
  -> Ptr.Ptr Void
  -> HsBindgen.Runtime.LibC.CSize
  -> IO FileOperationStatus
hs_bindgen_654858ed6a5db417 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_654858ed6a5db417_base

{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h 21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk ::
     Ptr.Ptr HsBindgen.Runtime.LibC.CFile
     -- ^ __C declaration:__ @file_ptr@
  -> Ptr.Ptr Void
     -- ^ __C declaration:__ @buffer@
  -> HsBindgen.Runtime.LibC.CSize
     -- ^ __C declaration:__ @bytes_to_read@
  -> IO FileOperationStatus
read_file_chunk = hs_bindgen_654858ed6a5db417
