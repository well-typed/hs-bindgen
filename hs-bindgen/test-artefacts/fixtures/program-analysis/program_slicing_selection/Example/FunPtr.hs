{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.read_file_chunk
    )
  where

import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_programanalysisprogram_slici_Example_get_read_file_chunk@
hs_bindgen_6e12e70d71890a10 :: IO (BG.FunPtr (BG.Ptr HsBindgen.Runtime.LibC.CFile -> BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus))
hs_bindgen_6e12e70d71890a10 =
  BG.fromFFIType hs_bindgen_6e12e70d71890a10_base

{-# NOINLINE read_file_chunk #-}
{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h 21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk :: BG.FunPtr (BG.Ptr HsBindgen.Runtime.LibC.CFile -> BG.Ptr BG.Void -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus)
read_file_chunk =
  BG.unsafePerformIO hs_bindgen_6e12e70d71890a10
