{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.register_handler
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
