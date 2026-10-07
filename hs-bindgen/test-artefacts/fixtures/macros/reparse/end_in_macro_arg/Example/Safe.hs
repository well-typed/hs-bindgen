{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.f
    , Example.Safe.g
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/end_in_macro_arg.h>"
  , "T hs_bindgen_32975406bf2752f4 ("
  , "  T arg1"
  , ")"
  , "{"
  , "  return (f)(arg1);"
  , "}"
  , "signed int hs_bindgen_7b14389d4be48230 ("
  , "  U arg1"
  , ")"
  , "{"
  , "  return (g)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_f@
foreign import ccall safe "hs_bindgen_32975406bf2752f4" hs_bindgen_32975406bf2752f4_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_f@
hs_bindgen_32975406bf2752f4 ::
     T
  -> IO T
hs_bindgen_32975406bf2752f4 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_32975406bf2752f4_base (BG.toFFIType x0))

{-| __C declaration:__ @f@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 13:3@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
f ::
     T
     -- ^ __C declaration:__ @a@
  -> IO T
f = hs_bindgen_32975406bf2752f4

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_g@
foreign import ccall safe "hs_bindgen_7b14389d4be48230" hs_bindgen_7b14389d4be48230_base ::
     BG.CInt
  -> IO BG.CInt

-- __unique:__ @test_macrosreparseend_in_macro_ar_Example_Safe_g@
hs_bindgen_7b14389d4be48230 ::
     U
  -> IO BG.CInt
hs_bindgen_7b14389d4be48230 =
  \x0 ->
    fmap BG.fromFFIType (hs_bindgen_7b14389d4be48230_base (BG.toFFIType x0))

{-| __C declaration:__ @g@

    __defined at:__ @macros\/reparse\/end_in_macro_arg.h 20:5@

    __exported by:__ @macros\/reparse\/end_in_macro_arg.h@
-}
g ::
     U
     -- ^ __C declaration:__ @a@
  -> IO BG.CInt
g = hs_bindgen_7b14389d4be48230
