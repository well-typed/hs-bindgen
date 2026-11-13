{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/nested/struct_ffitype.h>"
  , "/* test_bindingspecsfun_argnesteds_Example_get_f */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_118355d08f21c4e1 (void)) ("
  , "  void (*arg1) ("
  , "  C arg1"
  , ")"
  , ")"
  , "{"
  , "  return &f;"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argnesteds_Example_get_f@
foreign import ccall unsafe "hs_bindgen_118355d08f21c4e1" hs_bindgen_118355d08f21c4e1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_bindingspecsfun_argnesteds_Example_get_f@
hs_bindgen_118355d08f21c4e1 :: IO (BG.FunPtr (BG.FunPtr (M.C -> IO ()) -> IO ()))
hs_bindgen_118355d08f21c4e1 =
  fmap BG.fromFFIType hs_bindgen_118355d08f21c4e1_base

{-# NOINLINE f #-}
{-| __C declaration:__ @f@

    __defined at:__ @binding-specs\/fun_arg\/nested\/struct_ffitype.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/nested\/struct_ffitype.h@
-}
f :: BG.FunPtr (BG.FunPtr (M.C -> IO ()) -> IO ())
f = BG.unsafePerformIO hs_bindgen_118355d08f21c4e1
