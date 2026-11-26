{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module DoxygenInLibraryHeaders.A.FunPtr
    ( DoxygenInLibraryHeaders.A.FunPtr.a_detach
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import DoxygenInLibraryHeaders.A
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <doxygen_in_library_headers.h>"
  , "/* test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.A_get_a_detach */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c13896268d8b2fac (void)) ("
  , "  struct a_node *arg1"
  , ")"
  , "{"
  , "  return &a_detach;"
  , "}"
  ]))

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.A_get_a_detach@
foreign import ccall unsafe "hs_bindgen_c13896268d8b2fac" hs_bindgen_c13896268d8b2fac_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.A_get_a_detach@
hs_bindgen_c13896268d8b2fac :: IO (BG.FunPtr (BG.Ptr A_node -> IO BG.CInt))
hs_bindgen_c13896268d8b2fac =
  fmap BG.fromFFIType hs_bindgen_c13896268d8b2fac_base

{-# NOINLINE a_detach #-}
{-| Detach a node from its list.

    [__@node@__]: The node to detach

    __Returns:__ 0 on success

    __C declaration:__ @a_detach@

    __defined at:__ @doxygen_in_library_headers\/a.h 27:5@

    __exported by:__ @doxygen_in_library_headers.h@
-}
a_detach :: BG.FunPtr (BG.Ptr A_node -> IO BG.CInt)
a_detach =
  BG.unsafePerformIO hs_bindgen_c13896268d8b2fac
