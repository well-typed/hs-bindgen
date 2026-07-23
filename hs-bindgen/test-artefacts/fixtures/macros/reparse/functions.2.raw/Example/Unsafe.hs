{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    , Example.Unsafe.baz
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/functions.h>"
  , "void hs_bindgen_63386173f0facb31 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_2ffb271b09dfbbf9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_3cf87e230ba213f9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_63386173f0facb31" hs_bindgen_63386173f0facb31_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_foo@
hs_bindgen_63386173f0facb31 ::
     BG.CInt
  -> IO ()
hs_bindgen_63386173f0facb31 =
  BG.fromFFIType hs_bindgen_63386173f0facb31_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_63386173f0facb31

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_2ffb271b09dfbbf9" hs_bindgen_2ffb271b09dfbbf9_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_bar@
hs_bindgen_2ffb271b09dfbbf9 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_2ffb271b09dfbbf9 =
  BG.fromFFIType hs_bindgen_2ffb271b09dfbbf9_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
bar = hs_bindgen_2ffb271b09dfbbf9

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_baz@
foreign import ccall unsafe "hs_bindgen_3cf87e230ba213f9" hs_bindgen_3cf87e230ba213f9_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Unsafe_baz@
hs_bindgen_3cf87e230ba213f9 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_3cf87e230ba213f9 =
  BG.fromFFIType hs_bindgen_3cf87e230ba213f9_base

{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
baz = hs_bindgen_3cf87e230ba213f9
