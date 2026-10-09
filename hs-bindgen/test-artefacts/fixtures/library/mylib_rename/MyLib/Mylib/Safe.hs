{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.Safe
    ( MyLib.Mylib.Safe.point_zero
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "void hs_bindgen_30eafe6420a354c8 ("
  , "  point_t *arg1"
  , ")"
  , "{"
  , "  *arg1 = (point_zero)();"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_rename_MyLib.Mylib_Safe_point_zero@
foreign import ccall safe "hs_bindgen_30eafe6420a354c8" hs_bindgen_30eafe6420a354c8_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarymylib_rename_MyLib.Mylib_Safe_point_zero@
hs_bindgen_30eafe6420a354c8 ::
     BG.Ptr MyLib.Mylib.Types.Point
  -> IO ()
hs_bindgen_30eafe6420a354c8 =
  \x0 ->
    hs_bindgen_30eafe6420a354c8_base (BG.toFFIType x0)

{-| __C declaration:__ @point_zero@

    __defined at:__ @mylib.h 6:9@

    __exported by:__ @mylib.h@
-}
point_zero :: IO MyLib.Mylib.Types.Point
point_zero =
  BG.allocaAndPeek (\res0 ->
                      hs_bindgen_30eafe6420a354c8 res0)
