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
  [ "#include <types/boot/internal_module.h>"
  , "/* test_typesbootinternal_module_Example_get_register_handler */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_445b9f20265aa78d (void)) ("
  , "  on_event arg1"
  , ")"
  , "{"
  , "  return &register_handler;"
  , "}"
  , "/* test_typesbootinternal_module_Example_get_visit */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c20b8f8652c06a3d (void)) ("
  , "  signed int (*arg1) ("
  , "  struct point *arg1"
  , ")"
  , ")"
  , "{"
  , "  return &visit;"
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

    __defined at:__ @types\/boot\/internal_module.h 23:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
register_handler :: BG.FunPtr (On_event -> IO ())
register_handler =
  BG.unsafePerformIO hs_bindgen_445b9f20265aa78d

-- __unique:__ @test_typesbootinternal_module_Example_get_visit@
foreign import ccall unsafe "hs_bindgen_c20b8f8652c06a3d" hs_bindgen_c20b8f8652c06a3d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typesbootinternal_module_Example_get_visit@
hs_bindgen_c20b8f8652c06a3d :: IO (BG.FunPtr (BG.FunPtr (BG.Ptr Point -> IO BG.CInt) -> IO ()))
hs_bindgen_c20b8f8652c06a3d =
  fmap BG.fromFFIType hs_bindgen_c20b8f8652c06a3d_base

{-# NOINLINE visit #-}
{-| __C declaration:__ @visit@

    __defined at:__ @types\/boot\/internal_module.h 34:6@

    __exported by:__ @types\/boot\/internal_module.h@
-}
visit :: BG.FunPtr (BG.FunPtr (BG.Ptr Point -> IO BG.CInt) -> IO ())
visit =
  BG.unsafePerformIO hs_bindgen_c20b8f8652c06a3d
