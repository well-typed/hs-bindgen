{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ModuleNames.Same_basename.FunPtr
    ( ModuleNames.Same_basename.FunPtr.same_basename_open
    , ModuleNames.Same_basename.FunPtr.same_basename_close
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import ModuleNames.Same_basename
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <module_names.h>"
  , "/* test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_open */"
  , "__attribute__ ((const))"
  , "same_basename (*hs_bindgen_15ee889619a9fdc0 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &same_basename_open;"
  , "}"
  , "/* test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_close */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_511af401061ea8c2 (void)) ("
  , "  same_basename arg1"
  , ")"
  , "{"
  , "  return &same_basename_close;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_open@
foreign import ccall unsafe "hs_bindgen_15ee889619a9fdc0" hs_bindgen_15ee889619a9fdc0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_open@
hs_bindgen_15ee889619a9fdc0 :: IO (BG.FunPtr (BG.CInt -> IO Same_basename))
hs_bindgen_15ee889619a9fdc0 =
  fmap BG.fromFFIType hs_bindgen_15ee889619a9fdc0_base

{-# NOINLINE same_basename_open #-}
{-| __C declaration:__ @same_basename_open@

    __defined at:__ @..\/same_basename.h 11:15@

    __exported by:__ @module_names.h@
-}
same_basename_open :: BG.FunPtr (BG.CInt -> IO Same_basename)
same_basename_open =
  BG.unsafePerformIO hs_bindgen_15ee889619a9fdc0

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_close@
foreign import ccall unsafe "hs_bindgen_511af401061ea8c2" hs_bindgen_511af401061ea8c2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_get_same_basename_close@
hs_bindgen_511af401061ea8c2 :: IO (BG.FunPtr (Same_basename -> IO ()))
hs_bindgen_511af401061ea8c2 =
  fmap BG.fromFFIType hs_bindgen_511af401061ea8c2_base

{-# NOINLINE same_basename_close #-}
{-| __C declaration:__ @same_basename_close@

    __defined at:__ @..\/same_basename.h 12:6@

    __exported by:__ @module_names.h@
-}
same_basename_close :: BG.FunPtr (Same_basename -> IO ())
same_basename_close =
  BG.unsafePerformIO hs_bindgen_511af401061ea8c2
