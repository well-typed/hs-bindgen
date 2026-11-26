{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module GeneratedSpecs.Same_header.Unsafe
    ( GeneratedSpecs.Same_header.Unsafe.same_header_get
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import GeneratedSpecs.Same_header
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <generated_specs.h>"
  , "double hs_bindgen_bfee0ca47e451a77 ("
  , "  same_header *arg1"
  , ")"
  , "{"
  , "  return (same_header_get)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Same_header_Unsafe_same_header_get@
foreign import ccall unsafe "hs_bindgen_bfee0ca47e451a77" hs_bindgen_bfee0ca47e451a77_base ::
     BG.Ptr BG.Void
  -> IO BG.CDouble

-- __unique:__ @test_librarybindingspecsgenerate_GeneratedSpecs.Same_header_Unsafe_same_header_get@
hs_bindgen_bfee0ca47e451a77 ::
     BG.Ptr Same_header
  -> IO BG.CDouble
hs_bindgen_bfee0ca47e451a77 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_bfee0ca47e451a77_base (BG.toFFIType x0))

{-| __C declaration:__ @same_header_get@

    __defined at:__ @generated_specs\/same_header.h 15:8@

    __exported by:__ @generated_specs.h@
-}
same_header_get ::
     Same_header
     -- ^ __C declaration:__ @s@
  -> IO BG.CDouble
same_header_get =
  \s0 ->
    BG.with s0 (\s1 -> hs_bindgen_bfee0ca47e451a77 s1)
