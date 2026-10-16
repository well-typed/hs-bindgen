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
  [ "#include <types/boot/internal_module.h>"
  , "void hs_bindgen_19e2c75d828a3989 ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  (register_handler)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_typesbootinternal_module_Example_Unsafe_register_handler@
foreign import ccall unsafe "hs_bindgen_19e2c75d828a3989" hs_bindgen_19e2c75d828a3989_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_typesbootinternal_module_Example_Unsafe_register_handler@
hs_bindgen_19e2c75d828a3989 ::
     On_event
  -> IO ()
hs_bindgen_19e2c75d828a3989 =
  \x0 ->
    hs_bindgen_19e2c75d828a3989_base (BG.toFFIType x0)

{-| __C declaration:__ @register_handler@

    __defined at:__ @types\/boot\/internal_module.h 16:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
register_handler ::
     On_event
     -- ^ __C declaration:__ @f@
  -> IO ()
register_handler = hs_bindgen_19e2c75d828a3989
