{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "T hs_bindgen_5f3d395b37b2aea3 ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_5f3d395b37b2aea3" hs_bindgen_5f3d395b37b2aea3_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_f@
hs_bindgen_5f3d395b37b2aea3 ::
     T
  -> IO T
hs_bindgen_5f3d395b37b2aea3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5f3d395b37b2aea3_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 12:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f ::
     T
     -- ^ __C declaration:__ @a@
  -> IO T
f = hs_bindgen_5f3d395b37b2aea3
