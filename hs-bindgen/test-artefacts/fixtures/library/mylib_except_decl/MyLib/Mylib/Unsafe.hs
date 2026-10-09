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
  , "void hs_bindgen_80fb26d0974dba76 ("
  , "  point_t *arg1"
  , ")"
  , "{"
  , "  *arg1 = (point_zero)();"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib_Unsafe_point_zero@
foreign import ccall unsafe "hs_bindgen_80fb26d0974dba76" hs_bindgen_80fb26d0974dba76_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib_Unsafe_point_zero@
hs_bindgen_80fb26d0974dba76 ::
     BG.Ptr MyLib.Mylib.Types.Point_t
  -> IO ()
hs_bindgen_80fb26d0974dba76 =
  \x0 ->
    hs_bindgen_80fb26d0974dba76_base (BG.toFFIType x0)

{-| __C declaration:__ @point_zero@

    __defined at:__ @mylib.h 6:9@

    __exported by:__ @mylib.h@
-}
point_zero :: IO MyLib.Mylib.Types.Point_t
point_zero =
  BG.allocaAndPeek (\res0 ->
                      hs_bindgen_80fb26d0974dba76 res0)
