{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Writer.Safe
    ( M.Writer.Safe.writer_write
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <root.h>"
  , "signed int hs_bindgen_790895a3e056b1b5 ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return (writer_write)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_reader_writer_M.Writer_Safe_writer_write@
foreign import ccall safe "hs_bindgen_790895a3e056b1b5" hs_bindgen_790895a3e056b1b5_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_reader_writer_M.Writer_Safe_writer_write@
hs_bindgen_790895a3e056b1b5 ::
     BG.Ptr M.Reader.Handle
  -> IO BG.CInt
hs_bindgen_790895a3e056b1b5 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_790895a3e056b1b5_base (BG.toFFIType x0))

{-| __C declaration:__ @writer_write@

    __defined at:__ @writer.h 5:5@

    __exported by:__ @root.h@
-}
writer_write ::
     BG.Ptr M.Reader.Handle
     -- ^ __C declaration:__ @h@
  -> IO BG.CInt
writer_write = hs_bindgen_790895a3e056b1b5
