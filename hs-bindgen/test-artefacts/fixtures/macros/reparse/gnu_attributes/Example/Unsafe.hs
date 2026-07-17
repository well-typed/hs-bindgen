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
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "void hs_bindgen_6716bcb05c1aff64 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_04dc8fc0c78d4f3a ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_8e06931d2ef7ccdb ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (dash)(arg1);"
  , "}"
  , "void hs_bindgen_ec7c3016706a46c2 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  , "void hs_bindgen_f6865acd1e6f7475 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (heq)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_6716bcb05c1aff64" hs_bindgen_6716bcb05c1aff64_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_foo@
hs_bindgen_6716bcb05c1aff64 ::
     BOOL
  -> IO ()
hs_bindgen_6716bcb05c1aff64 =
  BG.fromFFIType hs_bindgen_6716bcb05c1aff64_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo ::
     BOOL
  -> IO ()
foo = hs_bindgen_6716bcb05c1aff64

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_04dc8fc0c78d4f3a" hs_bindgen_04dc8fc0c78d4f3a_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_bar@
hs_bindgen_04dc8fc0c78d4f3a ::
     BOOL
  -> IO ()
hs_bindgen_04dc8fc0c78d4f3a =
  BG.fromFFIType hs_bindgen_04dc8fc0c78d4f3a_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar ::
     BOOL
  -> IO ()
bar = hs_bindgen_04dc8fc0c78d4f3a

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_dash@
foreign import ccall unsafe "hs_bindgen_8e06931d2ef7ccdb" hs_bindgen_8e06931d2ef7ccdb_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_dash@
hs_bindgen_8e06931d2ef7ccdb ::
     BOOL
  -> IO ()
hs_bindgen_8e06931d2ef7ccdb =
  BG.fromFFIType hs_bindgen_8e06931d2ef7ccdb_base

{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash ::
     BOOL
  -> IO ()
dash = hs_bindgen_8e06931d2ef7ccdb

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_ec7c3016706a46c2" hs_bindgen_ec7c3016706a46c2_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_quux@
hs_bindgen_ec7c3016706a46c2 ::
     BOOL
  -> IO ()
hs_bindgen_ec7c3016706a46c2 =
  BG.fromFFIType hs_bindgen_ec7c3016706a46c2_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux ::
     BOOL
  -> IO ()
quux = hs_bindgen_ec7c3016706a46c2

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_heq@
foreign import ccall unsafe "hs_bindgen_f6865acd1e6f7475" hs_bindgen_f6865acd1e6f7475_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Unsafe_heq@
hs_bindgen_f6865acd1e6f7475 ::
     BOOL
  -> IO ()
hs_bindgen_f6865acd1e6f7475 =
  BG.fromFFIType hs_bindgen_f6865acd1e6f7475_base

{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq ::
     BOOL
  -> IO ()
heq = hs_bindgen_f6865acd1e6f7475
