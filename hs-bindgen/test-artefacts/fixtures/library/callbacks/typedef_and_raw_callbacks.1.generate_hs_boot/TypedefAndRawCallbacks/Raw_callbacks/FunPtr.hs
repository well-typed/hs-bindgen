{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module TypedefAndRawCallbacks.Raw_callbacks.FunPtr
    ( TypedefAndRawCallbacks.Raw_callbacks.FunPtr.takes_typedef_callback
    , TypedefAndRawCallbacks.Raw_callbacks.FunPtr.takes_raw_callback
    , TypedefAndRawCallbacks.Raw_callbacks.FunPtr.takes_primitive_raw_callback
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified TypedefAndRawCallbacks.Typedef_callback
import Prelude (IO, fmap)
import TypedefAndRawCallbacks.Raw_callbacks

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <typedef_and_raw_callbacks.h>"
  , "/* test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_typedef_callback */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_cca3002c98a9493b (void)) ("
  , "  struct loop *arg1,"
  , "  typedef_callback arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  return &takes_typedef_callback;"
  , "}"
  , "/* test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_raw_callback */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e1f456af289dab56 (void)) ("
  , "  struct loop *arg1,"
  , "  void (*arg2) ("
  , "  struct loop *arg1,"
  , "  signed int arg2"
  , ")"
  , ")"
  , "{"
  , "  return &takes_raw_callback;"
  , "}"
  , "/* test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_primitive_raw_callback */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b376ff82ddb860ea (void)) ("
  , "  void (*arg1) ("
  , "  char const *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &takes_primitive_raw_callback;"
  , "}"
  ]))

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_typedef_callback@
foreign import ccall unsafe "hs_bindgen_cca3002c98a9493b" hs_bindgen_cca3002c98a9493b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_typedef_callback@
hs_bindgen_cca3002c98a9493b :: IO (BG.FunPtr (BG.Ptr Loop -> TypedefAndRawCallbacks.Typedef_callback.Typedef_callback -> BG.Ptr BG.Void -> IO ()))
hs_bindgen_cca3002c98a9493b =
  fmap BG.fromFFIType hs_bindgen_cca3002c98a9493b_base

{-# NOINLINE takes_typedef_callback #-}
{-| __C declaration:__ @takes_typedef_callback@

    __defined at:__ @raw_callbacks.h 13:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_typedef_callback :: BG.FunPtr (BG.Ptr Loop -> TypedefAndRawCallbacks.Typedef_callback.Typedef_callback -> BG.Ptr BG.Void -> IO ())
takes_typedef_callback =
  BG.unsafePerformIO hs_bindgen_cca3002c98a9493b

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_raw_callback@
foreign import ccall unsafe "hs_bindgen_e1f456af289dab56" hs_bindgen_e1f456af289dab56_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_raw_callback@
hs_bindgen_e1f456af289dab56 :: IO (BG.FunPtr (BG.Ptr Loop -> BG.FunPtr (BG.Ptr Loop -> BG.CInt -> IO ()) -> IO ()))
hs_bindgen_e1f456af289dab56 =
  fmap BG.fromFFIType hs_bindgen_e1f456af289dab56_base

{-# NOINLINE takes_raw_callback #-}
{-| __C declaration:__ @takes_raw_callback@

    __defined at:__ @raw_callbacks.h 14:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_raw_callback :: BG.FunPtr (BG.Ptr Loop -> BG.FunPtr (BG.Ptr Loop -> BG.CInt -> IO ()) -> IO ())
takes_raw_callback =
  BG.unsafePerformIO hs_bindgen_e1f456af289dab56

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_primitive_raw_callback@
foreign import ccall unsafe "hs_bindgen_b376ff82ddb860ea" hs_bindgen_b376ff82ddb860ea_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_get_takes_primitive_raw_callback@
hs_bindgen_b376ff82ddb860ea :: IO (BG.FunPtr (BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ()) -> IO ()))
hs_bindgen_b376ff82ddb860ea =
  fmap BG.fromFFIType hs_bindgen_b376ff82ddb860ea_base

{-# NOINLINE takes_primitive_raw_callback #-}
{-| __C declaration:__ @takes_primitive_raw_callback@

    __defined at:__ @raw_callbacks.h 17:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_primitive_raw_callback :: BG.FunPtr (BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ()) -> IO ())
takes_primitive_raw_callback =
  BG.unsafePerformIO hs_bindgen_b376ff82ddb860ea
