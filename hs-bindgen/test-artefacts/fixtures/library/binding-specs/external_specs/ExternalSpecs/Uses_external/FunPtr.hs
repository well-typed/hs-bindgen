{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module ExternalSpecs.Uses_external.FunPtr
    ( ExternalSpecs.Uses_external.FunPtr.uses_external_make
    )
  where

import qualified GeneratedSpecs.Other_header
import qualified GeneratedSpecs.Same_header
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import ExternalSpecs.Uses_external
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <external_specs.h>"
  , "/* test_librarybindingspecsexternal_ExternalSpecs.Uses_external_get_uses_external_make */"
  , "__attribute__ ((const))"
  , "struct uses_external (*hs_bindgen_74803e1905615713 (void)) ("
  , "  same_header arg1,"
  , "  other_header arg2"
  , ")"
  , "{"
  , "  return &uses_external_make;"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsexternal_ExternalSpecs.Uses_external_get_uses_external_make@
foreign import ccall unsafe "hs_bindgen_74803e1905615713" hs_bindgen_74803e1905615713_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarybindingspecsexternal_ExternalSpecs.Uses_external_get_uses_external_make@
hs_bindgen_74803e1905615713 :: IO (BG.FunPtr (GeneratedSpecs.Same_header.Same_header -> GeneratedSpecs.Other_header.Other_header -> IO Uses_external))
hs_bindgen_74803e1905615713 =
  fmap BG.fromFFIType hs_bindgen_74803e1905615713_base

{-# NOINLINE uses_external_make #-}
{-| __C declaration:__ @uses_external_make@

    __defined at:__ @external_specs\/uses_external.h 15:22@

    __exported by:__ @external_specs.h@
-}
uses_external_make :: BG.FunPtr (GeneratedSpecs.Same_header.Same_header -> GeneratedSpecs.Other_header.Other_header -> IO Uses_external)
uses_external_make =
  BG.unsafePerformIO hs_bindgen_74803e1905615713
