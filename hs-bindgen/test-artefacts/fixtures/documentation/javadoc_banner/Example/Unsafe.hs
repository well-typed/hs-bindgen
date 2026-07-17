{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.banner_double
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <documentation/javadoc_banner.h>"
  , "signed int hs_bindgen_3d6508e65f7dad3c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (banner_double)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_documentationjavadoc_banner_Example_Unsafe_banner_double@
foreign import ccall unsafe "hs_bindgen_3d6508e65f7dad3c" hs_bindgen_3d6508e65f7dad3c_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_documentationjavadoc_banner_Example_Unsafe_banner_double@
hs_bindgen_3d6508e65f7dad3c ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_3d6508e65f7dad3c =
  BG.fromFFIType hs_bindgen_3d6508e65f7dad3c_base

{-| A function documented with a Javadoc banner-style comment.

    The decorative asterisks should be stripped by doxygen, producing clean documentation without any '*' noise.

    [__@x@__]: The input value

    __Returns:__ The doubled value

    __C declaration:__ @banner_double@

    __defined at:__ @documentation\/javadoc_banner.h 14:5@

    __exported by:__ @documentation\/javadoc_banner.h@
-}
banner_double ::
     BG.CInt
     {- ^

          [__@x@__]: The input value

          __C declaration:__ @x@
     -}
  -> IO BG.CInt
banner_double = hs_bindgen_3d6508e65f7dad3c
