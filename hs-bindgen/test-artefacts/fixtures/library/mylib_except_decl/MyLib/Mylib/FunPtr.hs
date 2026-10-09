{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module MyLib.Mylib.FunPtr
    ( MyLib.Mylib.FunPtr.point_zero
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified MyLib.Mylib.Types
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <mylib.h>"
  , "/* test_librarymylib_except_decl_MyLib.Mylib_get_point_zero */"
  , "__attribute__ ((const))"
  , "point_t (*hs_bindgen_94cce1c449e6e0d1 (void)) (void)"
  , "{"
  , "  return &point_zero;"
  , "}"
  ]))

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib_get_point_zero@
foreign import ccall unsafe "hs_bindgen_94cce1c449e6e0d1" hs_bindgen_94cce1c449e6e0d1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarymylib_except_decl_MyLib.Mylib_get_point_zero@
hs_bindgen_94cce1c449e6e0d1 :: IO (BG.FunPtr (IO MyLib.Mylib.Types.Point_t))
hs_bindgen_94cce1c449e6e0d1 =
  fmap BG.fromFFIType hs_bindgen_94cce1c449e6e0d1_base

{-# NOINLINE point_zero #-}
{-| __C declaration:__ @point_zero@

    __defined at:__ @mylib.h 6:9@

    __exported by:__ @mylib.h@
-}
point_zero :: BG.FunPtr (IO MyLib.Mylib.Types.Point_t)
point_zero =
  BG.unsafePerformIO hs_bindgen_94cce1c449e6e0d1
