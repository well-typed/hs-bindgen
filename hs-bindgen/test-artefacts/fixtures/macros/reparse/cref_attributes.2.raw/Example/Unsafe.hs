{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    , Example.Unsafe.quux
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse/cref_attributes.h>"
  , "void hs_bindgen_bc0a3a9b7f66fcad ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_3e07344f1445a945 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_d67e55f386a01958 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_bc0a3a9b7f66fcad" hs_bindgen_bc0a3a9b7f66fcad_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_foo@
hs_bindgen_bc0a3a9b7f66fcad ::
     BG.CInt
  -> IO ()
hs_bindgen_bc0a3a9b7f66fcad =
  BG.fromFFIType hs_bindgen_bc0a3a9b7f66fcad_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/cref_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
foo ::
     BG.CInt
  -> IO ()
foo = hs_bindgen_bc0a3a9b7f66fcad

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_3e07344f1445a945" hs_bindgen_3e07344f1445a945_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_bar@
hs_bindgen_3e07344f1445a945 ::
     BG.CInt
  -> IO ()
hs_bindgen_3e07344f1445a945 =
  BG.fromFFIType hs_bindgen_3e07344f1445a945_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/cref_attributes.h 6:37@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
bar ::
     BG.CInt
  -> IO ()
bar = hs_bindgen_3e07344f1445a945

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_d67e55f386a01958" hs_bindgen_d67e55f386a01958_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_quux@
hs_bindgen_d67e55f386a01958 ::
     BG.CInt
  -> IO ()
hs_bindgen_d67e55f386a01958 =
  BG.fromFFIType hs_bindgen_d67e55f386a01958_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/cref_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
quux ::
     BG.CInt
  -> IO ()
quux = hs_bindgen_d67e55f386a01958
