{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.LibC
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_programanalysisprogram_slici_Example_get_read_file_chunk@
hs_bindgen_6e12e70d71890a10 :: IO (Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.LibC.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus))
hs_bindgen_6e12e70d71890a10 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6e12e70d71890a10_base

{-# NOINLINE read_file_chunk #-}
{-| __C declaration:__ @read_file_chunk@

    __defined at:__ @program-analysis\/program_slicing_selection.h 21:26@

    __exported by:__ @program-analysis\/program_slicing_selection.h@
-}
read_file_chunk :: Ptr.FunPtr ((Ptr.Ptr HsBindgen.Runtime.LibC.CFile) -> (Ptr.Ptr Void) -> HsBindgen.Runtime.LibC.CSize -> IO FileOperationStatus)
read_file_chunk =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6e12e70d71890a10
