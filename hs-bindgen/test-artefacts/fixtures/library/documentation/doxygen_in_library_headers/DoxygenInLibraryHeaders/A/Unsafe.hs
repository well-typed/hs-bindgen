{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module DoxygenInLibraryHeaders.A.Unsafe
    ( DoxygenInLibraryHeaders.A.Unsafe.a_detach
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import DoxygenInLibraryHeaders.A
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <doxygen_in_library_headers.h>"
  , "signed int hs_bindgen_a754755f342a6c3c ("
  , "  struct a_node *arg1"
  , ")"
  , "{"
  , "  return (a_detach)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.A_Unsafe_a_detach@
foreign import ccall unsafe "hs_bindgen_a754755f342a6c3c" hs_bindgen_a754755f342a6c3c_base ::
     BG.Ptr BG.Void
  -> IO BG.CInt

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.A_Unsafe_a_detach@
hs_bindgen_a754755f342a6c3c ::
     BG.Ptr A_node
  -> IO BG.CInt
hs_bindgen_a754755f342a6c3c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_a754755f342a6c3c_base (BG.toFFIType x0))

{-| Detach a node from its list.

    [__@node@__]: The node to detach

    __Returns:__ 0 on success

    __C declaration:__ @a_detach@

    __defined at:__ @doxygen_in_library_headers\/a.h 27:5@

    __exported by:__ @doxygen_in_library_headers.h@
-}
a_detach ::
     BG.Ptr A_node
     {- ^

          [__@node@__]: The node to detach

          __C declaration:__ @node@
     -}
  -> IO BG.CInt
a_detach = hs_bindgen_a754755f342a6c3c
