{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.a
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/array_param.h>"
  , "/* test_functionsfun_ptr_instancesar_Example_get_a */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a00185fdfc403c08 (void)) ("
  , "  void (*arg1) ("
  , "  T *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &a;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesar_Example_get_a@
foreign import ccall unsafe "hs_bindgen_a00185fdfc403c08" hs_bindgen_a00185fdfc403c08_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesar_Example_get_a@
hs_bindgen_a00185fdfc403c08 :: IO (BG.FunPtr (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()) -> IO ()))
hs_bindgen_a00185fdfc403c08 =
  fmap BG.fromFFIType hs_bindgen_a00185fdfc403c08_base

{-# NOINLINE a #-}
{-| __C declaration:__ @a@

    __defined at:__ @functions\/fun_ptr_instances\/array_param.h 6:6@

    __exported by:__ @functions\/fun_ptr_instances\/array_param.h@
-}
a :: BG.FunPtr (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ()) -> IO ())
a = BG.unsafePerformIO hs_bindgen_a00185fdfc403c08
