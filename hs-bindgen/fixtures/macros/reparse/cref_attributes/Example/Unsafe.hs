{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.bar
    , Example.Unsafe.quux
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/cref_attributes.h>"
  , "void hs_bindgen_bc0a3a9b7f66fcad ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_3e07344f1445a945 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_d67e55f386a01958 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_bc0a3a9b7f66fcad" hs_bindgen_bc0a3a9b7f66fcad_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_foo@
hs_bindgen_bc0a3a9b7f66fcad ::
     BOOL
  -> IO ()
hs_bindgen_bc0a3a9b7f66fcad =
  RIP.fromFFIType hs_bindgen_bc0a3a9b7f66fcad_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/cref_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
foo ::
     BOOL
  -> IO ()
foo = hs_bindgen_bc0a3a9b7f66fcad

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_bar@
foreign import ccall unsafe "hs_bindgen_3e07344f1445a945" hs_bindgen_3e07344f1445a945_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_bar@
hs_bindgen_3e07344f1445a945 ::
     BOOL
  -> IO ()
hs_bindgen_3e07344f1445a945 =
  RIP.fromFFIType hs_bindgen_3e07344f1445a945_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/cref_attributes.h 6:37@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
bar ::
     BOOL
  -> IO ()
bar = hs_bindgen_3e07344f1445a945

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_d67e55f386a01958" hs_bindgen_d67e55f386a01958_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Unsafe_quux@
hs_bindgen_d67e55f386a01958 ::
     BOOL
  -> IO ()
hs_bindgen_d67e55f386a01958 =
  RIP.fromFFIType hs_bindgen_d67e55f386a01958_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/cref_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
quux ::
     BOOL
  -> IO ()
quux = hs_bindgen_d67e55f386a01958
