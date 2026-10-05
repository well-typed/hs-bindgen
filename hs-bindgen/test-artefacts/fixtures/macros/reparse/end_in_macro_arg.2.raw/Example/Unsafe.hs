{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    , Example.Unsafe.g
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "signed int hs_bindgen_5f3d395b37b2aea3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  , "signed int hs_bindgen_4bdcd42db8457bfb ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (g)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_5f3d395b37b2aea3" hs_bindgen_5f3d395b37b2aea3_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_f@
hs_bindgen_5f3d395b37b2aea3 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_5f3d395b37b2aea3 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_5f3d395b37b2aea3_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 13:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f ::
     BG.CInt
     -- ^ __C declaration:__ @a@
  -> IO BG.CInt
f = hs_bindgen_5f3d395b37b2aea3

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_g@
foreign import ccall unsafe "hs_bindgen_4bdcd42db8457bfb" hs_bindgen_4bdcd42db8457bfb_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Unsafe_g@
hs_bindgen_4bdcd42db8457bfb ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_4bdcd42db8457bfb =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_4bdcd42db8457bfb_base (BG.toFFIType x0))

{-| __C declaration:__ @g@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 20:5@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
g ::
     BG.CInt
     -- ^ __C declaration:__ @a@
  -> IO BG.CInt
g = hs_bindgen_4bdcd42db8457bfb
