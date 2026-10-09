{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Writer.FunPtr
    ( M.Writer.FunPtr.writer_write
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Reader
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <excepted_type/root.h>"
  , "/* test_libraryexcepted_type_M.Writer_get_writer_write */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7595a2ea4dad1e98 (void)) ("
  , "  struct handle *arg1"
  , ")"
  , "{"
  , "  return &writer_write;"
  , "}"
  ]))

-- __unique:__ @test_libraryexcepted_type_M.Writer_get_writer_write@
foreign import ccall unsafe "hs_bindgen_7595a2ea4dad1e98" hs_bindgen_7595a2ea4dad1e98_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryexcepted_type_M.Writer_get_writer_write@
hs_bindgen_7595a2ea4dad1e98 :: IO (BG.FunPtr (BG.Ptr M.Reader.Handle -> IO BG.CInt))
hs_bindgen_7595a2ea4dad1e98 =
  fmap BG.fromFFIType hs_bindgen_7595a2ea4dad1e98_base

{-# NOINLINE writer_write #-}
{-| __C declaration:__ @writer_write@

    __defined at:__ @writer.h 7:5@

    __exported by:__ @excepted_type\/root.h@
-}
writer_write :: BG.FunPtr (BG.Ptr M.Reader.Handle -> IO BG.CInt)
writer_write =
  BG.unsafePerformIO hs_bindgen_7595a2ea4dad1e98
