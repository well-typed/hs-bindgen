{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ModuleNames.C2d.Leading_digit.FunPtr
    ( ModuleNames.C2d.Leading_digit.FunPtr.leading_digit_fill
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified ModuleNames.Dash_name
import qualified ModuleNames.Same_basename
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <module_names.h>"
  , "/* test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_fill */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e9b63da656be0c05 (void)) ("
  , "  same_basename arg1,"
  , "  struct dash_name arg2"
  , ")"
  , "{"
  , "  return &leading_digit_fill;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_fill@
foreign import ccall unsafe "hs_bindgen_e9b63da656be0c05" hs_bindgen_e9b63da656be0c05_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_fill@
hs_bindgen_e9b63da656be0c05 :: IO (BG.FunPtr (ModuleNames.Same_basename.Same_basename -> ModuleNames.Dash_name.Dash_name -> IO ()))
hs_bindgen_e9b63da656be0c05 =
  fmap BG.fromFFIType hs_bindgen_e9b63da656be0c05_base

{-# NOINLINE leading_digit_fill #-}
{-| __C declaration:__ @leading_digit_fill@

    __defined at:__ @module_names\/2d\/leading_digit.h 13:6@

    __exported by:__ @module_names.h@
-}
leading_digit_fill :: BG.FunPtr (ModuleNames.Same_basename.Same_basename -> ModuleNames.Dash_name.Dash_name -> IO ())
leading_digit_fill =
  BG.unsafePerformIO hs_bindgen_e9b63da656be0c05
