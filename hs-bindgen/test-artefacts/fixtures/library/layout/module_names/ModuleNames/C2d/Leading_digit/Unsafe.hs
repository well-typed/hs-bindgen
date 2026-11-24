{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ModuleNames.C2d.Leading_digit.Unsafe
    ( ModuleNames.C2d.Leading_digit.Unsafe.leading_digit_fill
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified ModuleNames.Dash_name
import qualified ModuleNames.Same_basename
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <module_names.h>"
  , "void hs_bindgen_f4df2a1e9a895fb6 ("
  , "  same_basename arg1,"
  , "  struct dash_name *arg2"
  , ")"
  , "{"
  , "  (leading_digit_fill)(arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_Unsafe_leading_digit_fill@
foreign import ccall unsafe "hs_bindgen_f4df2a1e9a895fb6" hs_bindgen_f4df2a1e9a895fb6_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_Unsafe_leading_digit_fill@
hs_bindgen_f4df2a1e9a895fb6 ::
     ModuleNames.Same_basename.Same_basename
  -> BG.Ptr ModuleNames.Dash_name.Dash_name
  -> IO ()
hs_bindgen_f4df2a1e9a895fb6 =
  \x0 ->
    \x1 ->
      hs_bindgen_f4df2a1e9a895fb6_base (BG.toFFIType x0) (BG.toFFIType x1)

{-| __C declaration:__ @leading_digit_fill@

    __defined at:__ @module_names\/2d\/leading_digit.h 13:6@

    __exported by:__ @module_names.h@
-}
leading_digit_fill ::
     ModuleNames.Same_basename.Same_basename
     -- ^ __C declaration:__ @s@
  -> ModuleNames.Dash_name.Dash_name
     -- ^ __C declaration:__ @value@
  -> IO ()
leading_digit_fill =
  \s0 ->
    \value1 ->
      BG.with value1 (\value2 ->
                        hs_bindgen_f4df2a1e9a895fb6 s0 value2)
