{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module TypedefAndRawCallbacks.Raw_callbacks.Unsafe
    ( TypedefAndRawCallbacks.Raw_callbacks.Unsafe.takes_typedef_callback
    , TypedefAndRawCallbacks.Raw_callbacks.Unsafe.takes_raw_callback
    , TypedefAndRawCallbacks.Raw_callbacks.Unsafe.takes_primitive_raw_callback
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified TypedefAndRawCallbacks.Typedef_callback
import Prelude (IO)
import TypedefAndRawCallbacks.Raw_callbacks

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <typedef_and_raw_callbacks.h>"
  , "void hs_bindgen_92a1d66d78559048 ("
  , "  struct loop *arg1,"
  , "  typedef_callback arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  (takes_typedef_callback)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_f2e8f005c24b7cd9 ("
  , "  struct loop *arg1,"
  , "  void (*arg2) ("
  , "  struct loop *arg1,"
  , "  signed int arg2"
  , ")"
  , ")"
  , "{"
  , "  (takes_raw_callback)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_54102d8c21683df2 ("
  , "  void (*arg1) ("
  , "  char const *arg1"
  , ")"
  , ")"
  , "{"
  , "  (takes_primitive_raw_callback)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_typedef_callback@
foreign import ccall unsafe "hs_bindgen_92a1d66d78559048" hs_bindgen_92a1d66d78559048_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_typedef_callback@
hs_bindgen_92a1d66d78559048 ::
     BG.Ptr Loop
  -> TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_92a1d66d78559048 =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_92a1d66d78559048_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

{-| __C declaration:__ @takes_typedef_callback@

    __defined at:__ @raw_callbacks.h 13:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_typedef_callback ::
     BG.Ptr Loop
     -- ^ __C declaration:__ @l@
  -> TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
     -- ^ __C declaration:__ @callback@
  -> BG.Ptr BG.Void
     -- ^ __C declaration:__ @user@
  -> IO ()
takes_typedef_callback = hs_bindgen_92a1d66d78559048

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_raw_callback@
foreign import ccall unsafe "hs_bindgen_f2e8f005c24b7cd9" hs_bindgen_f2e8f005c24b7cd9_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_raw_callback@
hs_bindgen_f2e8f005c24b7cd9 ::
     BG.Ptr Loop
  -> BG.FunPtr (BG.Ptr Loop -> BG.CInt -> IO ())
  -> IO ()
hs_bindgen_f2e8f005c24b7cd9 =
  \x0 ->
    \x1 ->
      hs_bindgen_f2e8f005c24b7cd9_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @takes_raw_callback@

    __defined at:__ @raw_callbacks.h 14:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_raw_callback ::
     BG.Ptr Loop
     -- ^ __C declaration:__ @l@
  -> BG.FunPtr (BG.Ptr Loop -> BG.CInt -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
takes_raw_callback = hs_bindgen_f2e8f005c24b7cd9

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_primitive_raw_callback@
foreign import ccall unsafe "hs_bindgen_54102d8c21683df2" hs_bindgen_54102d8c21683df2_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Unsafe_takes_primitive_raw_callback@
hs_bindgen_54102d8c21683df2 ::
     BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ())
  -> IO ()
hs_bindgen_54102d8c21683df2 =
  \x0 ->
    hs_bindgen_54102d8c21683df2_base (BG.toFFIType x0)

{-| __C declaration:__ @takes_primitive_raw_callback@

    __defined at:__ @raw_callbacks.h 17:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_primitive_raw_callback ::
     BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
takes_primitive_raw_callback =
  hs_bindgen_54102d8c21683df2
