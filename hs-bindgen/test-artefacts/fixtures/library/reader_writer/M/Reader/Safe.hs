{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Reader.Safe
    ( M.Reader.Safe.reader_read
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "signed int hs_bindgen_2460962656052ecf ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return (reader_read)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_reader_writer_M.Reader_Safe_reader_read@
foreign import ccall safe "hs_bindgen_2460962656052ecf" hs_bindgen_2460962656052ecf_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_reader_writer_M.Reader_Safe_reader_read@
hs_bindgen_2460962656052ecf ::
     BG.Ptr Handle
  -> IO BG.CInt
hs_bindgen_2460962656052ecf =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_2460962656052ecf_base (BG.toFFIType x0))

{-| __C declaration:__ @reader_read@

    __defined at:__ @reader.h 5:5@

    __exported by:__ @root.h@
-}
reader_read ::
     BG.Ptr Handle
     -- ^ __C declaration:__ @h@
  -> IO BG.CInt
reader_read = hs_bindgen_2460962656052ecf
