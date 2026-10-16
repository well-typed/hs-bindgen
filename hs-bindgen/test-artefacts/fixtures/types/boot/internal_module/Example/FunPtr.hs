{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.register_handler
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/boot/internal_module.h>"
  , "/* test_typesbootinternal_module_Example_get_register_handler */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_445b9f20265aa78d (void)) ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  return &register_handler;"
  , "}"
  ]))

-- __unique:__ @test_typesbootinternal_module_Example_get_register_handler@
foreign import ccall unsafe "hs_bindgen_445b9f20265aa78d" hs_bindgen_445b9f20265aa78d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesbootinternal_module_Example_get_register_handler@
hs_bindgen_445b9f20265aa78d :: IO (BG.FunPtr (On_event -> IO ()))
hs_bindgen_445b9f20265aa78d =
  fmap BG.fromFFIType hs_bindgen_445b9f20265aa78d_base

{-# NOINLINE register_handler #-}
{-| __C declaration:__ @register_handler@

    __defined at:__ @types\/boot\/internal_module.h 16:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
register_handler :: BG.FunPtr (On_event -> IO ())
register_handler =
  BG.unsafePerformIO hs_bindgen_445b9f20265aa78d
