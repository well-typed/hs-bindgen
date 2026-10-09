{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Writer.Unsafe
    ( M.Writer.Unsafe.writer_write
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <excepted_type/root.h>"
  , "signed int hs_bindgen_1dfc4b265618c88b ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return (writer_write)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_libraryexcepted_type_M.Writer_Unsafe_writer_write@
foreign import ccall unsafe "hs_bindgen_1dfc4b265618c88b" hs_bindgen_1dfc4b265618c88b_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_libraryexcepted_type_M.Writer_Unsafe_writer_write@
hs_bindgen_1dfc4b265618c88b ::
     BG.Ptr M.Reader.Handle
  -> IO BG.CInt
hs_bindgen_1dfc4b265618c88b =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_1dfc4b265618c88b_base (BG.toFFIType x0))

{-| __C declaration:__ @writer_write@

    __defined at:__ @writer.h 7:5@

    __exported by:__ @excepted_type\/root.h@
-}
writer_write ::
     BG.Ptr M.Reader.Handle
     -- ^ __C declaration:__ @h@
  -> IO BG.CInt
writer_write = hs_bindgen_1dfc4b265618c88b
