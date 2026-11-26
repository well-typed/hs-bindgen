{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module TypedefAndRawCallbacks.Raw_callbacks.Safe
    ( TypedefAndRawCallbacks.Raw_callbacks.Safe.takes_typedef_callback
    , TypedefAndRawCallbacks.Raw_callbacks.Safe.takes_raw_callback
    , TypedefAndRawCallbacks.Raw_callbacks.Safe.takes_primitive_raw_callback
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
  , "void hs_bindgen_68aa85ebc1990c7d ("
  , "  struct loop *arg1,"
  , "  typedef_callback arg2,"
  , "  void *arg3"
  , ")"
  , "{"
  , "  (takes_typedef_callback)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_b240da91b7e68e87 ("
  , "  struct loop *arg1,"
  , "  void (*arg2) ("
  , "  struct loop *arg1,"
  , "  signed int arg2"
  , ")"
  , ")"
  , "{"
  , "  (takes_raw_callback)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_b7a9bbf3c8eb0a40 ("
  , "  void (*arg1) ("
  , "  char const *arg1"
  , ")"
  , ")"
  , "{"
  , "  (takes_primitive_raw_callback)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_typedef_callback@
foreign import ccall safe "hs_bindgen_68aa85ebc1990c7d" hs_bindgen_68aa85ebc1990c7d_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_typedef_callback@
hs_bindgen_68aa85ebc1990c7d ::
     BG.Ptr Loop
  -> TypedefAndRawCallbacks.Typedef_callback.Typedef_callback
  -> BG.Ptr BG.Void
  -> IO ()
hs_bindgen_68aa85ebc1990c7d =
  \x0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_68aa85ebc1990c7d_base (BG.toFFIType x0) (BG.toFFIType x1) (BG.toFFIType x2)

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
takes_typedef_callback = hs_bindgen_68aa85ebc1990c7d

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_raw_callback@
foreign import ccall safe "hs_bindgen_b240da91b7e68e87" hs_bindgen_b240da91b7e68e87_base ::
     BG.Ptr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_raw_callback@
hs_bindgen_b240da91b7e68e87 ::
     BG.Ptr Loop
  -> BG.FunPtr (BG.Ptr Loop -> BG.CInt -> IO ())
  -> IO ()
hs_bindgen_b240da91b7e68e87 =
  \x0 ->
    \x1 ->
      hs_bindgen_b240da91b7e68e87_base (BG.toFFIType x0) (BG.toFFIType x1)

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
takes_raw_callback = hs_bindgen_b240da91b7e68e87

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_primitive_raw_callback@
foreign import ccall safe "hs_bindgen_b7a9bbf3c8eb0a40" hs_bindgen_b7a9bbf3c8eb0a40_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_librarycallbackstypedef_and__TypedefAndRawCallbacks.Raw_callbacks_Safe_takes_primitive_raw_callback@
hs_bindgen_b7a9bbf3c8eb0a40 ::
     BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ())
  -> IO ()
hs_bindgen_b7a9bbf3c8eb0a40 =
  \x0 ->
    hs_bindgen_b7a9bbf3c8eb0a40_base (BG.toFFIType x0)

{-| __C declaration:__ @takes_primitive_raw_callback@

    __defined at:__ @raw_callbacks.h 17:6@

    __exported by:__ @typedef_and_raw_callbacks.h@
-}
takes_primitive_raw_callback ::
     BG.FunPtr (PtrConst.PtrConst BG.CChar -> IO ())
     -- ^ __C declaration:__ @callback@
  -> IO ()
takes_primitive_raw_callback =
  hs_bindgen_b7a9bbf3c8eb0a40
