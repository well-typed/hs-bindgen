{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module DoxygenInLibraryHeaders.B.Global
    ( DoxygenInLibraryHeaders.B.Global.b_default
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import DoxygenInLibraryHeaders.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <doxygen_in_library_headers.h>"
  , "/* test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.B_get_b_default */"
  , "__attribute__ ((const))"
  , "struct b_list *hs_bindgen_58f12a563a874c0b (void)"
  , "{"
  , "  return &b_default;"
  , "}"
  ]))

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.B_get_b_default@
foreign import ccall unsafe "hs_bindgen_58f12a563a874c0b" hs_bindgen_58f12a563a874c0b_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarydocumentationdoxygen__DoxygenInLibraryHeaders.B_get_b_default@
hs_bindgen_58f12a563a874c0b :: IO (BG.Ptr B_list)
hs_bindgen_58f12a563a874c0b =
  fmap BG.fromFFIType hs_bindgen_58f12a563a874c0b_base

{-# NOINLINE b_default #-}
{-| The list that new nodes join

    __C declaration:__ @b_default@

    __defined at:__ @b.h 10:22@

    __exported by:__ @doxygen_in_library_headers.h@
-}
b_default :: BG.Ptr B_list
b_default =
  BG.unsafePerformIO hs_bindgen_58f12a563a874c0b
