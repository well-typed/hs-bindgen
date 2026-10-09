{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module M.Register.FunPtr
    ( M.Register.FunPtr.register_cb
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M.Foo
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <callback/register.h>"
  , "/* test_librarycallback_M.Register_get_register_cb */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_171f1cb0bf29a291 (void)) ("
  , "  void (*arg1) ("
  , "  struct foo *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &register_cb;"
  , "}"
  ]))

-- __unique:__ @test_librarycallback_M.Register_get_register_cb@
foreign import ccall unsafe "hs_bindgen_171f1cb0bf29a291" hs_bindgen_171f1cb0bf29a291_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycallback_M.Register_get_register_cb@
hs_bindgen_171f1cb0bf29a291 :: IO (BG.FunPtr (BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ()) -> IO ()))
hs_bindgen_171f1cb0bf29a291 =
  fmap BG.fromFFIType hs_bindgen_171f1cb0bf29a291_base

{-# NOINLINE register_cb #-}
{-| __C declaration:__ @register_cb@

    __defined at:__ @callback\/register.h 5:6@

    __exported by:__ @callback\/register.h@
-}
register_cb :: BG.FunPtr (BG.FunPtr (BG.Ptr M.Foo.Foo -> IO ()) -> IO ())
register_cb =
  BG.unsafePerformIO hs_bindgen_171f1cb0bf29a291
