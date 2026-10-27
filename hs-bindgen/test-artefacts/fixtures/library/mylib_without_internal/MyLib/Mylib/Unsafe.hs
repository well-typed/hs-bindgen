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
import qualified MyLib.Mylib.Types
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "void hs_bindgen_c23b3c97ba1fbc91 ("
  , "  point_t *arg1"
  , ")"
  , "{"
  , "  *arg1 = (point_zero)();"
  , "}"
  ]))

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib_Unsafe_point_zero@
foreign import ccall unsafe "hs_bindgen_c23b3c97ba1fbc91" hs_bindgen_c23b3c97ba1fbc91_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_mylib_without_internal_MyLib.Mylib_Unsafe_point_zero@
hs_bindgen_c23b3c97ba1fbc91 ::
     BG.Ptr MyLib.Mylib.Types.Point_t
  -> IO ()
hs_bindgen_c23b3c97ba1fbc91 =
  \x0 ->
    hs_bindgen_c23b3c97ba1fbc91_base (BG.toFFIType x0)

{-| __C declaration:__ @point_zero@

    __defined at:__ @mylib.h 7:9@

    __exported by:__ @mylib.h@
-}
point_zero :: IO MyLib.Mylib.Types.Point_t
point_zero =
  BG.allocaAndPeek (\res0 ->
                      hs_bindgen_c23b3c97ba1fbc91 res0)
