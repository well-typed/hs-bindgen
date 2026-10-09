{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Register.Safe
    ( M.Register.Safe.register_cb
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Foo
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <callback/register.h>"
  , "void hs_bindgen_428113be3fbeeb2e ("
  , "  void (*arg1) ("
  , "  struct foo *arg1"
  , ")"
  , ")"
  , "{"
  , "  (register_cb)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycallback_M.Register_Safe_register_cb@
foreign import ccall safe "hs_bindgen_428113be3fbeeb2e" hs_bindgen_428113be3fbeeb2e_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallback_M.Register_Safe_register_cb@
hs_bindgen_428113be3fbeeb2e ::
     BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ())
  -> IO ()
hs_bindgen_428113be3fbeeb2e =
  \x0 ->
    hs_bindgen_428113be3fbeeb2e_base (BG.toFFIType x0)

{-| __C declaration:__ @register_cb@

    __defined at:__ @callback\/register.h 5:6@

    __exported by:__ @callback\/register.h@
-}
register_cb ::
     BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ())
     -- ^ __C declaration:__ @cb@
  -> IO ()
register_cb = hs_bindgen_428113be3fbeeb2e
