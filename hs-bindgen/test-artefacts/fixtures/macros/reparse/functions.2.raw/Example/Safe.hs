{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    , Example.Safe.baz
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/functions.h>"
  , "void hs_bindgen_afd9c4a8467f5418 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "signed int hs_bindgen_368818873d354fc5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (bar)(arg1);"
  , "}"
  , "signed int hs_bindgen_61f10d22d53c81ac ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (baz)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_afd9c4a8467f5418" hs_bindgen_afd9c4a8467f5418_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_foo@
hs_bindgen_afd9c4a8467f5418 ::
     BG.CInt
  -> IO ()
hs_bindgen_afd9c4a8467f5418 =
  BG.fromFFIType hs_bindgen_afd9c4a8467f5418_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/functions.h 7:6@

    __exported by:__ @macros\/reparse\/functions.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_afd9c4a8467f5418

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_368818873d354fc5" hs_bindgen_368818873d354fc5_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_bar@
hs_bindgen_368818873d354fc5 ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_368818873d354fc5 =
  BG.fromFFIType hs_bindgen_368818873d354fc5_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/functions.h 9:19@

    __exported by:__ @macros\/reparse\/functions.h@
-}
bar ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
bar = hs_bindgen_368818873d354fc5

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_baz@
foreign import ccall safe "hs_bindgen_61f10d22d53c81ac" hs_bindgen_61f10d22d53c81ac_base ::
     BG.Int32
  -> IO BG.Int32

-- __unique:__ @test_macrosreparsefunctions_2_raw_Example_Safe_baz@
hs_bindgen_61f10d22d53c81ac ::
     BG.CInt
  -> IO BG.CInt
hs_bindgen_61f10d22d53c81ac =
  BG.fromFFIType hs_bindgen_61f10d22d53c81ac_base

{-| __C declaration:__ @baz@

    __defined at:__ @macros\/reparse\/functions.h 14:12@

    __exported by:__ @macros\/reparse\/functions.h@
-}
baz ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
baz = hs_bindgen_61f10d22d53c81ac
