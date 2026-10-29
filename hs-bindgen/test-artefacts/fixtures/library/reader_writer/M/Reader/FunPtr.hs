{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Reader.FunPtr
    ( M.Reader.FunPtr.reader_read
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "/* test_reader_writer_M.Reader_get_reader_read */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_1c4984992f2f6fbe (void)) ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return &reader_read;"
  , "}"
  ]))

-- __unique:__ @test_reader_writer_M.Reader_get_reader_read@
foreign import ccall unsafe "hs_bindgen_1c4984992f2f6fbe" hs_bindgen_1c4984992f2f6fbe_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_reader_writer_M.Reader_get_reader_read@
hs_bindgen_1c4984992f2f6fbe :: IO (BG.FunPtr (BG.Ptr Handle -> IO BG.CInt))
hs_bindgen_1c4984992f2f6fbe =
  fmap BG.fromFFIType hs_bindgen_1c4984992f2f6fbe_base

{-# NOINLINE reader_read #-}
{-| __C declaration:__ @reader_read@

    __defined at:__ @reader.h 5:5@

    __exported by:__ @root.h@
-}
reader_read :: BG.FunPtr (BG.Ptr Handle -> IO BG.CInt)
reader_read =
  BG.unsafePerformIO hs_bindgen_1c4984992f2f6fbe
