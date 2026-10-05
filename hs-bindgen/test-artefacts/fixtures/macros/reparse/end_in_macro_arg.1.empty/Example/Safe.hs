{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "signed int hs_bindgen_32975406bf2752f4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_f@
foreign import ccall safe "hs_bindgen_32975406bf2752f4" hs_bindgen_32975406bf2752f4_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_f@
hs_bindgen_32975406bf2752f4 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_32975406bf2752f4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_32975406bf2752f4_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 12:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f ::
     BG.CInt
     -- ^ __C declaration:__ @a@
  -> IO BG.CInt
f = hs_bindgen_32975406bf2752f4
