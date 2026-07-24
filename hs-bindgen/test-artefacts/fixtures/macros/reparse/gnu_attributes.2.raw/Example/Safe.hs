{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    , Example.Safe.dash
    , Example.Safe.quux
    , Example.Safe.heq
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "void hs_bindgen_5e910042f66236ac ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_912eee72881dada9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_90648744cbb16108 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (dash)(arg1);"
  , "}"
  , "void hs_bindgen_3bf2453c79c79342 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  , "void hs_bindgen_89e85ad8f7343241 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (heq)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_foo@
foreign import ccall safe "hs_bindgen_5e910042f66236ac" hs_bindgen_5e910042f66236ac_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_foo@
hs_bindgen_5e910042f66236ac ::
     BG.CInt
  -> IO ()
hs_bindgen_5e910042f66236ac =
  BG.fromFFIType hs_bindgen_5e910042f66236ac_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo ::
     BG.CInt
  -> IO ()
foo = hs_bindgen_5e910042f66236ac

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_bar@
foreign import ccall safe "hs_bindgen_912eee72881dada9" hs_bindgen_912eee72881dada9_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_bar@
hs_bindgen_912eee72881dada9 ::
     BG.CInt
  -> IO ()
hs_bindgen_912eee72881dada9 =
  BG.fromFFIType hs_bindgen_912eee72881dada9_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar ::
     BG.CInt
  -> IO ()
bar = hs_bindgen_912eee72881dada9

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_dash@
foreign import ccall safe "hs_bindgen_90648744cbb16108" hs_bindgen_90648744cbb16108_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_dash@
hs_bindgen_90648744cbb16108 ::
     BG.CInt
  -> IO ()
hs_bindgen_90648744cbb16108 =
  BG.fromFFIType hs_bindgen_90648744cbb16108_base

{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash ::
     BG.CInt
  -> IO ()
dash = hs_bindgen_90648744cbb16108

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_quux@
foreign import ccall safe "hs_bindgen_3bf2453c79c79342" hs_bindgen_3bf2453c79c79342_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_quux@
hs_bindgen_3bf2453c79c79342 ::
     BG.CInt
  -> IO ()
hs_bindgen_3bf2453c79c79342 =
  BG.fromFFIType hs_bindgen_3bf2453c79c79342_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux ::
     BG.CInt
  -> IO ()
quux = hs_bindgen_3bf2453c79c79342

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_heq@
foreign import ccall safe "hs_bindgen_89e85ad8f7343241" hs_bindgen_89e85ad8f7343241_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Safe_heq@
hs_bindgen_89e85ad8f7343241 ::
     BG.CInt
  -> IO ()
hs_bindgen_89e85ad8f7343241 =
  BG.fromFFIType hs_bindgen_89e85ad8f7343241_base

{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq ::
     BG.CInt
  -> IO ()
heq = hs_bindgen_89e85ad8f7343241
