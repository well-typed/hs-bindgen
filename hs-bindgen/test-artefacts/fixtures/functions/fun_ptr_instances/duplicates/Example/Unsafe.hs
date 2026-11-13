{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.d
    , Example.Unsafe.e
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/duplicates.h>"
  , "void hs_bindgen_93eb7cb73ea7709d ("
  , "  void (*arg1) ("
  , "  T const arg1"
  , "),"
  , "  void (*arg2) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  (d)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_5c262dc098f92ec7 ("
  , "  void (*arg1) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , "),"
  , "  void (*arg2) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , ")"
  , "{"
  , "  (e)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_Unsafe_d@
foreign import ccall unsafe "hs_bindgen_93eb7cb73ea7709d" hs_bindgen_93eb7cb73ea7709d_base ::
     BG.FunPtr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_Unsafe_d@
hs_bindgen_93eb7cb73ea7709d ::
     BG.FunPtr (T -> IO ())
  -> BG.FunPtr (T -> IO ())
  -> IO ()
hs_bindgen_93eb7cb73ea7709d =
  \x0 ->
    \x1 ->
      hs_bindgen_93eb7cb73ea7709d_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @d@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 6:6@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
d ::
     BG.FunPtr (T -> IO ())
     -- ^ __C declaration:__ @a@
  -> BG.FunPtr (T -> IO ())
     -- ^ __C declaration:__ @b@
  -> IO ()
d = hs_bindgen_93eb7cb73ea7709d

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_Unsafe_e@
foreign import ccall unsafe "hs_bindgen_5c262dc098f92ec7" hs_bindgen_5c262dc098f92ec7_base ::
     BG.FunPtr BG.Void
  -> BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesdu_Example_Unsafe_e@
hs_bindgen_5c262dc098f92ec7 ::
     BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
  -> BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
  -> IO ()
hs_bindgen_5c262dc098f92ec7 =
  \x0 ->
    \x1 ->
      hs_bindgen_5c262dc098f92ec7_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @e@

    __defined at:__ @functions\/fun_ptr_instances\/duplicates.h 9:6@

    __exported by:__ @functions\/fun_ptr_instances\/duplicates.h@
-}
e ::
     BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
     -- ^ __C declaration:__ @a@
  -> BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
     -- ^ __C declaration:__ @c@
  -> IO ()
e = hs_bindgen_5c262dc098f92ec7
