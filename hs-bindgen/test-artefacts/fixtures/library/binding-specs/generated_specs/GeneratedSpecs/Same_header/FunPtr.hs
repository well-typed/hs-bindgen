{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module GeneratedSpecs.Same_header.FunPtr
    ( GeneratedSpecs.Same_header.FunPtr.same_header_get
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import GeneratedSpecs.Same_header
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <generated_specs.h>"
  , "/* test_librarybindingspecsgenerate_GeneratedSpecs.Same_header_get_same_header_get */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_e7b74d2f390e763e (void)) ("
  , "  same_header arg1"
  , ")"
  , "{"
  , "  return &same_header_get;"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Same_header_get_same_header_get@
foreign import ccall unsafe "hs_bindgen_e7b74d2f390e763e" hs_bindgen_e7b74d2f390e763e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Same_header_get_same_header_get@
hs_bindgen_e7b74d2f390e763e :: IO (BG.FunPtr (Same_header -> IO BG.CDouble))
hs_bindgen_e7b74d2f390e763e =
  fmap BG.fromFFIType hs_bindgen_e7b74d2f390e763e_base

{-# NOINLINE same_header_get #-}
{-| __C declaration:__ @same_header_get@

    __defined at:__ @generated_specs\/same_header.h 15:8@

    __exported by:__ @generated_specs.h@
-}
same_header_get :: BG.FunPtr (Same_header -> IO BG.CDouble)
same_header_get =
  BG.unsafePerformIO hs_bindgen_e7b74d2f390e763e
