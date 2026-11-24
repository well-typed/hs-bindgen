{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module TypedefAndRawCallbacks.Same_raw_type.Safe
    ( TypedefAndRawCallbacks.Same_raw_type.Safe.also_takes_raw_callback
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified TypedefAndRawCallbacks.Raw_callbacks
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <typedef_and_raw_callbacks.h>"
  , "void hs_bindgen_6700dfcd4798d84d ("
  , "  struct loop *arg1,"
  , "  void (*arg2) ("
  , "  struct loop *arg1,"
  , "  signed int arg2"
  , ")"
  , ")"
  , "{"
  , "  (also_takes_raw_callback)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Same_raw_type_Safe_also_takes_raw_callback@
foreign import ccall safe "hs_bindgen_6700dfcd4798d84d" hs_bindgen_6700dfcd4798d84d_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Same_raw_type_Safe_also_takes_raw_callback@
hs_bindgen_6700dfcd4798d84d ::
     BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop
  -> BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ())
  -> IO ()
hs_bindgen_6700dfcd4798d84d =
  \x0 ->
    \x1 ->
      hs_bindgen_6700dfcd4798d84d_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @also_takes_raw_callback@

    __defined at:__ @typedef_and_raw_callbacks\/same_raw_type.h 8:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
also_takes_raw_callback ::
     BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop
     -- ^ __C declaration:__ @l@
  -> BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
also_takes_raw_callback = hs_bindgen_6700dfcd4798d84d
