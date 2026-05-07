{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.banner_double
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <documentation/javadoc_banner.h>"
  , "signed int hs_bindgen_7d3fd818d0780e11 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (banner_double)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_documentationjavadoc_banner_Example_Safe_banner_double@
foreign import ccall safe "hs_bindgen_7d3fd818d0780e11" hs_bindgen_7d3fd818d0780e11_base ::
     RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_documentationjavadoc_banner_Example_Safe_banner_double@
hs_bindgen_7d3fd818d0780e11 ::
     RIP.CInt
  -> IO RIP.CInt
hs_bindgen_7d3fd818d0780e11 =
  RIP.fromFFIType hs_bindgen_7d3fd818d0780e11_base

{-| A function documented with a Javadoc banner-style comment.

    The decorative asterisks should be stripped by doxygen, producing clean documentation without any '*' noise.

    [__@x@__]: The input value

    __Returns:__ The doubled value

    __C declaration:__ @banner_double@

    __defined at:__ @documentation\/javadoc_banner.h 14:5@

    __exported by:__ @documentation\/javadoc_banner.h@
-}
banner_double ::
     RIP.CInt
     {- ^

          [__@x@__]: The input value

          __C declaration:__ @x@
     -}
  -> IO RIP.CInt
banner_double = hs_bindgen_7d3fd818d0780e11
