{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ModuleNames.C2d.Leading_digit.Global
    ( ModuleNames.C2d.Leading_digit.Global.leading_digit_count
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <module_names.h>"
  , "/* test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_count */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_25d489c047515269 (void)"
  , "{"
  , "  return &leading_digit_count;"
  , "}"
  ]))

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_count@
foreign import ccall unsafe "hs_bindgen_25d489c047515269" hs_bindgen_25d489c047515269_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_librarylayoutmodule_names_ModuleNames.C2d.Leading_digit_get_leading_digit_count@
hs_bindgen_25d489c047515269 :: IO (BG.Ptr BG.CInt)
hs_bindgen_25d489c047515269 =
  fmap BG.fromFFIType hs_bindgen_25d489c047515269_base

{-# NOINLINE leading_digit_count #-}
{-| __C declaration:__ @leading_digit_count@

    __defined at:__ @module_names\/2d\/leading_digit.h 15:12@

    __exported by:__ @module_names.h@
-}
leading_digit_count :: BG.Ptr BG.CInt
leading_digit_count =
  BG.unsafePerformIO hs_bindgen_25d489c047515269
