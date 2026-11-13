{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.a
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/array_param.h>"
  , "void hs_bindgen_e18829d03b61c1a3 ("
  , "  void (*arg1) ("
  , "  T *arg1"
  , ")"
  , ")"
  , "{"
  , "  (a)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesar_Example_Safe_a@
foreign import ccall safe "hs_bindgen_e18829d03b61c1a3" hs_bindgen_e18829d03b61c1a3_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesar_Example_Safe_a@
hs_bindgen_e18829d03b61c1a3 ::
     BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())
  -> IO ()
hs_bindgen_e18829d03b61c1a3 =
  \x0 ->
    hs_bindgen_e18829d03b61c1a3_base (BG.toFFIType x0)

{-| __C declaration:__ @a@

    __defined at:__ @functions\/fun_ptr_instances\/array_param.h 6:6@

    __exported by:__ @functions\/fun_ptr_instances\/array_param.h@
-}
a ::
     BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 3 T)) -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
a = hs_bindgen_e18829d03b61c1a3
