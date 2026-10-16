{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.register_handler
    , Example.FunPtr.visit
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/boot/ffi_module.h>"
  , "/* test_typesbootffi_module_Example_get_register_handler */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f9c0a75f96e98b0b (void)) ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  return &register_handler;"
  , "}"
  , "/* test_typesbootffi_module_Example_get_visit */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_95661d5d617bffad (void)) ("
  , "  signed int (*arg1) ("
  , "  struct point *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &visit;"
  , "}"
  ]))

-- __unique:__ @test_typesbootffi_module_Example_get_register_handler@
foreign import ccall unsafe "hs_bindgen_f9c0a75f96e98b0b" hs_bindgen_f9c0a75f96e98b0b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesbootffi_module_Example_get_register_handler@
hs_bindgen_f9c0a75f96e98b0b :: IO (BG.FunPtr (On_event -> IO ()))
hs_bindgen_f9c0a75f96e98b0b =
  fmap BG.fromFFIType hs_bindgen_f9c0a75f96e98b0b_base

{-# NOINLINE register_handler #-}
{-| __C declaration:__ @register_handler@

    __defined at:__ @types\/boot\/ffi_module.h 15:6@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
register_handler :: BG.FunPtr (On_event -> IO ())
register_handler =
  BG.unsafePerformIO hs_bindgen_f9c0a75f96e98b0b

-- __unique:__ @test_typesbootffi_module_Example_get_visit@
foreign import ccall unsafe "hs_bindgen_95661d5d617bffad" hs_bindgen_95661d5d617bffad_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesbootffi_module_Example_get_visit@
hs_bindgen_95661d5d617bffad :: IO (BG.FunPtr (BG.FunPtr (BG.Ptr Point -> IO BG.CInt) -> IO ()))
hs_bindgen_95661d5d617bffad =
  fmap BG.fromFFIType hs_bindgen_95661d5d617bffad_base

{-# NOINLINE visit #-}
{-| __C declaration:__ @visit@

    __defined at:__ @types\/boot\/ffi_module.h 26:6@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
visit :: BG.FunPtr (BG.FunPtr (BG.Ptr Point -> IO BG.CInt) -> IO ())
visit =
  BG.unsafePerformIO hs_bindgen_95661d5d617bffad
