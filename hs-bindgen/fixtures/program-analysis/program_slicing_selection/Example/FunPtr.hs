{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
foreign import ccall unsafe "hs_bindgen_6e12e70d71890a10" hs_bindgen_6e12e70d71890a10_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_programanalysisprogram_slici_Example_get_read_file_chunk@
hs_bindgen_6e12e70d71890a10 :: IO (RIP.FunPtr ((RIP.Ptr HsBindgen.Runtime.LibC.CFile) -> (RIP.Ptr RIP.Void) -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus))
hs_bindgen_6e12e70d71890a10 =
  RIP.fromFFIType hs_bindgen_6e12e70d71890a10_base

{-# NOINLINE read_file_chunk #-}
{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h 21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk :: RIP.FunPtr ((RIP.Ptr HsBindgen.Runtime.LibC.CFile) -> (RIP.Ptr RIP.Void) -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus)
read_file_chunk =
  RIP.unsafePerformIO hs_bindgen_6e12e70d71890a10
