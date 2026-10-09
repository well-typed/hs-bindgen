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
  [ "#include <excepted_type/root.h>"
  , "/* test_libraryexcepted_type_M.Reader_get_reader_read */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8131236d74460d92 (void)) ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return &reader_read;"
  , "}"
  ]))

-- __unique:__ @test_libraryexcepted_type_M.Reader_get_reader_read@
foreign import ccall unsafe "hs_bindgen_8131236d74460d92" hs_bindgen_8131236d74460d92_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryexcepted_type_M.Reader_get_reader_read@
hs_bindgen_8131236d74460d92 :: IO (BG.FunPtr (BG.Ptr Handle -> IO BG.CInt))
hs_bindgen_8131236d74460d92 =
  fmap BG.fromFFIType hs_bindgen_8131236d74460d92_base

{-# NOINLINE reader_read #-}
{-| __C declaration:__ @reader_read@

    __defined at:__ @reader.h 7:5@

    __exported by:__ @excepted_type\/root.h@
-}
reader_read :: BG.FunPtr (BG.Ptr Handle -> IO BG.CInt)
reader_read =
  BG.unsafePerformIO hs_bindgen_8131236d74460d92
