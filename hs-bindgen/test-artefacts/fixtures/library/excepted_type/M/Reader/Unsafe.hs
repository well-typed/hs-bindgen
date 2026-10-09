{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Reader.Unsafe
    ( M.Reader.Unsafe.reader_read
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <excepted_type/root.h>"
  , "signed int hs_bindgen_952ea9d1d8aaa010 ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return (reader_read)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryexcepted_type_M.Reader_Unsafe_reader_read@
foreign import ccall unsafe "hs_bindgen_952ea9d1d8aaa010" hs_bindgen_952ea9d1d8aaa010_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_libraryexcepted_type_M.Reader_Unsafe_reader_read@
hs_bindgen_952ea9d1d8aaa010 ::
     BG.Ptr Handle
  -> IO BG.CInt
hs_bindgen_952ea9d1d8aaa010 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_952ea9d1d8aaa010_base (BG.toFFIType x0))

{-| __C declaration:__ @reader_read@

    __defined at:__ @reader.h 7:5@

    __exported by:__ @excepted_type\/root.h@
-}
reader_read ::
     BG.Ptr Handle
     -- ^ __C declaration:__ @h@
  -> IO BG.CInt
reader_read = hs_bindgen_952ea9d1d8aaa010
