{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.A.FunPtr
    ( M.A.FunPtr.shape_draw
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.B
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <b.h>"
  , "/* test_forward_decl_M.A_get_shape_draw */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6d9cc9dfb4ee53a4 (void)) ("
  , "  struct shape *arg1"
  , ")"
  , "{"
  , "  return &shape_draw;"
  , "}"
  ]))

-- __unique:__ @test_forward_decl_M.A_get_shape_draw@
foreign import ccall unsafe "hs_bindgen_6d9cc9dfb4ee53a4" hs_bindgen_6d9cc9dfb4ee53a4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_forward_decl_M.A_get_shape_draw@
hs_bindgen_6d9cc9dfb4ee53a4 :: IO (BG.FunPtr (BG.Ptr M.B.Shape -> IO ()))
hs_bindgen_6d9cc9dfb4ee53a4 =
  fmap BG.fromFFIType hs_bindgen_6d9cc9dfb4ee53a4_base

{-# NOINLINE shape_draw #-}
{-| __C declaration:__ @shape_draw@

    __defined at:__ @a.h 5:6@

    __exported by:__ @b.h@
-}
shape_draw :: BG.FunPtr (BG.Ptr M.B.Shape -> IO ())
shape_draw =
  BG.unsafePerformIO hs_bindgen_6d9cc9dfb4ee53a4
