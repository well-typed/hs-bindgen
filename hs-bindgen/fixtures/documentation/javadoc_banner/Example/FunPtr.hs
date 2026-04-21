{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.banner_double
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <documentation/javadoc_banner.h>"
  , "/* test_documentationjavadoc_banner_Example_get_banner_double */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_57ce95431493ea3f (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &banner_double;"
  , "}"
  ]))

-- __unique:__ @test_documentationjavadoc_banner_Example_get_banner_double@
foreign import ccall unsafe "hs_bindgen_57ce95431493ea3f" hs_bindgen_57ce95431493ea3f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_documentationjavadoc_banner_Example_get_banner_double@
hs_bindgen_57ce95431493ea3f :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_57ce95431493ea3f =
  RIP.fromFFIType hs_bindgen_57ce95431493ea3f_base

{-# NOINLINE banner_double #-}
{-| A function documented with a Javadoc banner-style comment.

    The decorative asterisks should be stripped by doxygen, producing clean documentation without any '*' noise.

    [__@x@__]: The input value

    __Returns:__ The doubled value

    __C declaration:__ @banner_double@

    __defined at:__ @documentation\/javadoc_banner.h 14:5@

    __exported by:__ @documentation\/javadoc_banner.h@
-}
banner_double :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
banner_double =
  RIP.unsafePerformIO hs_bindgen_57ce95431493ea3f
