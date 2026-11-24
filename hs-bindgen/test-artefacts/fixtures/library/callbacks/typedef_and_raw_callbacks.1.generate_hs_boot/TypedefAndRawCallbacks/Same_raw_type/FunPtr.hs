{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module TypedefAndRawCallbacks.Same_raw_type.FunPtr
    ( TypedefAndRawCallbacks.Same_raw_type.FunPtr.also_takes_raw_callback
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified TypedefAndRawCallbacks.Raw_callbacks
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <typedef_and_raw_callbacks.h>"
  , "/* test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Same_raw_type_get_also_takes_raw_callback */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f0d69cbe0732cceb (void)) ("
  , "  struct loop *arg1,"
  , "  void (*arg2) ("
  , "  struct loop *arg1,"
  , "  signed int arg2"
  , ")"
  , ")"
  , "{"
  , "  return &also_takes_raw_callback;"
  , "}"
  ]))

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Same_raw_type_get_also_takes_raw_callback@
foreign import ccall unsafe "hs_bindgen_f0d69cbe0732cceb" hs_bindgen_f0d69cbe0732cceb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Same_raw_type_get_also_takes_raw_callback@
hs_bindgen_f0d69cbe0732cceb :: IO (BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()) -> IO ()))
hs_bindgen_f0d69cbe0732cceb =
  fmap BG.fromFFIType hs_bindgen_f0d69cbe0732cceb_base

{-# NOINLINE also_takes_raw_callback #-}
{-| __C declaration:__ @also_takes_raw_callback@

    __defined at:__ @typedef_and_raw_callbacks\/same_raw_type.h 8:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
also_takes_raw_callback :: BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.FunPtr (BG.Ptr TypedefAndRawCallbacks.Raw_callbacks.Loop -> BG.CInt -> IO ()) -> IO ())
also_takes_raw_callback =
  BG.unsafePerformIO hs_bindgen_f0d69cbe0732cceb
