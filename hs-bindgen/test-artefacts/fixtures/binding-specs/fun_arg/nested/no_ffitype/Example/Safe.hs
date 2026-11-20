{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/nested/no_ffitype.h>"
  , "void hs_bindgen_9188aad048dc5880 ("
  , "  void (*arg1) ("
  , "  C arg1,"
  , "  T arg2"
  , ")"
  , ")"
  , "{"
  , "  (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argnestedn_Example_Safe_f@
foreign import ccall safe "hs_bindgen_9188aad048dc5880" hs_bindgen_9188aad048dc5880_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argnestedn_Example_Safe_f@
hs_bindgen_9188aad048dc5880 ::
     BG.FunPtr (M.C -> T -> IO ())
  -> IO ()
hs_bindgen_9188aad048dc5880 =
  \x0 ->
    hs_bindgen_9188aad048dc5880_base (BG.toFFIType x0)

{-| __C declaration:__ @f@

    __defined at:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h@
-}
f ::
     BG.FunPtr (M.C -> T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_9188aad048dc5880
