{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.register_handler
    , Example.Unsafe.visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/boot/ffi_module.h>"
  , "void hs_bindgen_92d50ec97bf2edec ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  (register_handler)(arg1);"
  , "}"
  , "void hs_bindgen_05d58a9e27209620 ("
  , "  signed int (*arg1) ("
  , "  struct point *arg1"
  , ")"
  , ")"
  , "{"
  , "  (visit)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesbootffi_module_Example_Unsafe_register_handler@
foreign import ccall unsafe "hs_bindgen_92d50ec97bf2edec" hs_bindgen_92d50ec97bf2edec_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_typesbootffi_module_Example_Unsafe_register_handler@
hs_bindgen_92d50ec97bf2edec ::
     On_event
  -> IO ()
hs_bindgen_92d50ec97bf2edec =
  \x0 ->
    hs_bindgen_92d50ec97bf2edec_base (BG.toFFIType x0)

{-| __C declaration:__ @register_handler@

    __defined at:__ @types\/boot\/ffi_module.h 15:6@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
register_handler ::
     On_event
     -- ^ __C declaration:__ @f@
  -> IO ()
register_handler = hs_bindgen_92d50ec97bf2edec

-- __unique:__ @test_typesbootffi_module_Example_Unsafe_visit@
foreign import ccall unsafe "hs_bindgen_05d58a9e27209620" hs_bindgen_05d58a9e27209620_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_typesbootffi_module_Example_Unsafe_visit@
hs_bindgen_05d58a9e27209620 ::
     BG.FunPtr (BG.Ptr Point -> IO BG.CInt)
  -> IO ()
hs_bindgen_05d58a9e27209620 =
  \x0 ->
    hs_bindgen_05d58a9e27209620_base (BG.toFFIType x0)

{-| __C declaration:__ @visit@

    __defined at:__ @types\/boot\/ffi_module.h 26:6@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
visit ::
     BG.FunPtr (BG.Ptr Point -> IO BG.CInt)
     -- ^ __C declaration:__ @f@
  -> IO ()
visit = hs_bindgen_05d58a9e27209620
