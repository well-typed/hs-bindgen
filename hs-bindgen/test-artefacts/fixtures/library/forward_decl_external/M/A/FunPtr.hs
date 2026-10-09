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
import qualified Vendor.Shape
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <forward_decl/b.h>"
  , "/* test_libraryforward_decl_external_M.A_get_shape_draw */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e25df16256440a31 (void)) ("
  , "  struct shape *arg1"
  , ")"
  , "{"
  , "  return &shape_draw;"
  , "}"
  ]))

-- __unique:__ @test_libraryforward_decl_external_M.A_get_shape_draw@
foreign import ccall unsafe "hs_bindgen_e25df16256440a31" hs_bindgen_e25df16256440a31_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_libraryforward_decl_external_M.A_get_shape_draw@
hs_bindgen_e25df16256440a31 :: IO (BG.FunPtr (BG.Ptr Vendor.Shape.Shape -> IO ()))
hs_bindgen_e25df16256440a31 =
  fmap BG.fromFFIType hs_bindgen_e25df16256440a31_base

{-# NOINLINE shape_draw #-}
{-| __C declaration:__ @shape_draw@

    __defined at:__ @a.h 4:6@

    __exported by:__ @forward_decl\/b.h@
-}
shape_draw :: BG.FunPtr (BG.Ptr Vendor.Shape.Shape -> IO ())
shape_draw =
  BG.unsafePerformIO hs_bindgen_e25df16256440a31
