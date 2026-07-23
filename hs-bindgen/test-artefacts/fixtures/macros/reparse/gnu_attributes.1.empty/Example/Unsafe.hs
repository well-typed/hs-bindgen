{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    , Example.Unsafe.dash
    , Example.Unsafe.quux
    , Example.Unsafe.heq
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "void hs_bindgen_2794b70af4ccd43b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_01135a4a8f379826 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_0d59701db0fc10c2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (dash)(arg1);"
  , "}"
  , "void hs_bindgen_fc3ab959400ab9c8 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  , "void hs_bindgen_fa747bfe2b529e61 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (heq)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_2794b70af4ccd43b" hs_bindgen_2794b70af4ccd43b_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_foo@
hs_bindgen_2794b70af4ccd43b ::
     BG.CInt
  -> IO ()
hs_bindgen_2794b70af4ccd43b =
  BG.fromFFIType hs_bindgen_2794b70af4ccd43b_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo ::
     BG.CInt
  -> IO ()
foo = hs_bindgen_2794b70af4ccd43b

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_01135a4a8f379826" hs_bindgen_01135a4a8f379826_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_bar@
hs_bindgen_01135a4a8f379826 ::
     BG.CInt
  -> IO ()
hs_bindgen_01135a4a8f379826 =
  BG.fromFFIType hs_bindgen_01135a4a8f379826_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar ::
     BG.CInt
  -> IO ()
bar = hs_bindgen_01135a4a8f379826

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_dash@
foreign import ccall unsafe "hs_bindgen_0d59701db0fc10c2" hs_bindgen_0d59701db0fc10c2_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_dash@
hs_bindgen_0d59701db0fc10c2 ::
     BG.CInt
  -> IO ()
hs_bindgen_0d59701db0fc10c2 =
  BG.fromFFIType hs_bindgen_0d59701db0fc10c2_base

{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash ::
     BG.CInt
  -> IO ()
dash = hs_bindgen_0d59701db0fc10c2

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_fc3ab959400ab9c8" hs_bindgen_fc3ab959400ab9c8_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_quux@
hs_bindgen_fc3ab959400ab9c8 ::
     BG.CInt
  -> IO ()
hs_bindgen_fc3ab959400ab9c8 =
  BG.fromFFIType hs_bindgen_fc3ab959400ab9c8_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux ::
     BG.CInt
  -> IO ()
quux = hs_bindgen_fc3ab959400ab9c8

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_heq@
foreign import ccall unsafe "hs_bindgen_fa747bfe2b529e61" hs_bindgen_fa747bfe2b529e61_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes__Example_Unsafe_heq@
hs_bindgen_fa747bfe2b529e61 ::
     BG.CInt
  -> IO ()
hs_bindgen_fa747bfe2b529e61 =
  BG.fromFFIType hs_bindgen_fa747bfe2b529e61_base

{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq ::
     BG.CInt
  -> IO ()
heq = hs_bindgen_fa747bfe2b529e61
