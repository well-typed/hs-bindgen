{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module GeneratedSpecs.Other_header.FunPtr
    ( GeneratedSpecs.Other_header.FunPtr.other_header_make
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import GeneratedSpecs.Other_header
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <generated_specs.h>"
  , "/* test_librarybindingspecsgenerate_GeneratedSpecs.Other_header_get_other_header_make */"
  , "__attribute__ ((const))"
  , "other_header (*hs_bindgen_452783bf13911d80 (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &other_header_make;"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Other_header_get_other_header_make@
foreign import ccall unsafe "hs_bindgen_452783bf13911d80" hs_bindgen_452783bf13911d80_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Other_header_get_other_header_make@
hs_bindgen_452783bf13911d80 :: IO (BG.FunPtr (BG.CDouble -> IO Other_header))
hs_bindgen_452783bf13911d80 =
  fmap BG.fromFFIType hs_bindgen_452783bf13911d80_base

{-# NOINLINE other_header_make #-}
{-| __C declaration:__ @other_header_make@

    __defined at:__ @generated_specs\/other_header.h 10:14@

    __exported by:__ @generated_specs.h@
-}
other_header_make :: BG.FunPtr (BG.CDouble -> IO Other_header)
other_header_make =
  BG.unsafePerformIO hs_bindgen_452783bf13911d80
