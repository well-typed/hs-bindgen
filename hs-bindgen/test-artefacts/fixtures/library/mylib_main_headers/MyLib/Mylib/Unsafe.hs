{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Unsafe
    ( MyLib.Mylib.Unsafe.point_zero
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import MyLib.Mylib
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "void hs_bindgen_684cf5d1b0b31304 ("
  , "  point_t *arg1"
  , ")"
  , "{"
  , "  *arg1 = (point_zero)();"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_main_headers_MyLib.Mylib_Unsafe_point_zero@
foreign import ccall unsafe "hs_bindgen_684cf5d1b0b31304" hs_bindgen_684cf5d1b0b31304_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarymylib_main_headers_MyLib.Mylib_Unsafe_point_zero@
hs_bindgen_684cf5d1b0b31304 ::
     BG.Ptr Point_t
  -> IO ()
hs_bindgen_684cf5d1b0b31304 =
  \x0 ->
    hs_bindgen_684cf5d1b0b31304_base (BG.toFFIType x0)

{-| __C declaration:__ @point_zero@

    __defined at:__ @mylib.h 6:9@

    __exported by:__ @mylib.h@
-}
point_zero :: IO Point_t
point_zero =
  BG.allocaAndPeek (\res0 ->
                      hs_bindgen_684cf5d1b0b31304 res0)
