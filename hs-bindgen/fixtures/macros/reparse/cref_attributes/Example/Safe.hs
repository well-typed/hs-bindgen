{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    , Example.Safe.quux
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/cref_attributes.h>"
  , "void hs_bindgen_9f4b1c3748016429 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_183028e559c4eff2 ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_e02b651508844cbb ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_9f4b1c3748016429" hs_bindgen_9f4b1c3748016429_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_foo@
hs_bindgen_9f4b1c3748016429 ::
     BOOL
  -> IO ()
hs_bindgen_9f4b1c3748016429 =
  RIP.fromFFIType hs_bindgen_9f4b1c3748016429_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/cref_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
foo ::
     BOOL
  -> IO ()
foo = hs_bindgen_9f4b1c3748016429

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_183028e559c4eff2" hs_bindgen_183028e559c4eff2_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_bar@
hs_bindgen_183028e559c4eff2 ::
     BOOL
  -> IO ()
hs_bindgen_183028e559c4eff2 =
  RIP.fromFFIType hs_bindgen_183028e559c4eff2_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/cref_attributes.h 6:37@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
bar ::
     BOOL
  -> IO ()
bar = hs_bindgen_183028e559c4eff2

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_quux@
foreign import ccall safe "hs_bindgen_e02b651508844cbb" hs_bindgen_e02b651508844cbb_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsecref_attributes_Example_Safe_quux@
hs_bindgen_e02b651508844cbb ::
     BOOL
  -> IO ()
hs_bindgen_e02b651508844cbb =
  RIP.fromFFIType hs_bindgen_e02b651508844cbb_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/cref_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/cref_attributes.h@
-}
quux ::
     BOOL
  -> IO ()
quux = hs_bindgen_e02b651508844cbb
