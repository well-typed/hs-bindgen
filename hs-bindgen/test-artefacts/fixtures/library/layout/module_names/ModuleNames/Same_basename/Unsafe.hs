{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ModuleNames.Same_basename.Unsafe
    ( ModuleNames.Same_basename.Unsafe.same_basename_open
    , ModuleNames.Same_basename.Unsafe.same_basename_close
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import ModuleNames.Same_basename
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <module_names.h>"
  , "same_basename hs_bindgen_fa0ef33fecb83e3c ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (same_basename_open)(arg1);"
  , "}"
  , "void hs_bindgen_c687d6396e675fb2 ("
  , "  same_basename arg1"
  , ")"
  , "{"
  , "  (same_basename_close)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_Unsafe_same_basename_open@
foreign import ccall unsafe "hs_bindgen_fa0ef33fecb83e3c" hs_bindgen_fa0ef33fecb83e3c_base ::
     BG.CInt
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_Unsafe_same_basename_open@
hs_bindgen_fa0ef33fecb83e3c ::
     BG.CInt
  -> IO Same_basename
hs_bindgen_fa0ef33fecb83e3c =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_fa0ef33fecb83e3c_base (BG.toFFIType x0))

{-| __C declaration:__ @same_basename_open@

    __defined at:__ @..\/same_basename.h 11:15@

    __exported by:__ @module_names.h@
-}
same_basename_open ::
     BG.CInt
     -- ^ __C declaration:__ @width@
  -> IO Same_basename
same_basename_open = hs_bindgen_fa0ef33fecb83e3c

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_Unsafe_same_basename_close@
foreign import ccall unsafe "hs_bindgen_c687d6396e675fb2" hs_bindgen_c687d6396e675fb2_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.Same_basename_Unsafe_same_basename_close@
hs_bindgen_c687d6396e675fb2 ::
     Same_basename
  -> IO ()
hs_bindgen_c687d6396e675fb2 =
  \x0 ->
    hs_bindgen_c687d6396e675fb2_base (BG.toFFIType x0)

{-| __C declaration:__ @same_basename_close@

    __defined at:__ @..\/same_basename.h 12:6@

    __exported by:__ @module_names.h@
-}
same_basename_close ::
     Same_basename
     -- ^ __C declaration:__ @s@
  -> IO ()
same_basename_close = hs_bindgen_c687d6396e675fb2
