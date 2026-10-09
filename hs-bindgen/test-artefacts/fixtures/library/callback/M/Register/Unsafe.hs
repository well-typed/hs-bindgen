{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Register.Unsafe
    ( M.Register.Unsafe.register_cb
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Foo
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <callback/register.h>"
  , "void hs_bindgen_ac7dcebbc25e8001 ("
  , "  void (*arg1) ("
  , "  struct foo *arg1"
  , ")"
  , ")"
  , "{"
  , "  (register_cb)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycallback_M.Register_Unsafe_register_cb@
foreign import ccall unsafe "hs_bindgen_ac7dcebbc25e8001" hs_bindgen_ac7dcebbc25e8001_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallback_M.Register_Unsafe_register_cb@
hs_bindgen_ac7dcebbc25e8001 ::
     BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ())
  -> IO ()
hs_bindgen_ac7dcebbc25e8001 =
  \x0 ->
    hs_bindgen_ac7dcebbc25e8001_base (BG.toFFIType x0)

{-| __C declaration:__ @register_cb@

    __defined at:__ @callback\/register.h 5:6@

    __exported by:__ @callback\/register.h@
-}
register_cb ::
     BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ())
     -- ^ __C declaration:__ @cb@
  -> IO ()
register_cb = hs_bindgen_ac7dcebbc25e8001
