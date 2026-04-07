{-# LANGUAGE CApiFFI #-}
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

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse/gnu_attributes.h>"
  , "void hs_bindgen_702437d53bf7e32f ("
  , "  BOOL arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_432c6480e8c1355b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_b7cd534964204668 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (dash)(arg1);"
  , "}"
  , "void hs_bindgen_24c242811e7f290b ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (quux)(arg1);"
  , "}"
  , "void hs_bindgen_9bea1bb33c269bc2 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (heq)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_702437d53bf7e32f" hs_bindgen_702437d53bf7e32f_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_foo@
hs_bindgen_702437d53bf7e32f ::
     BOOL
  -> IO ()
hs_bindgen_702437d53bf7e32f =
  RIP.fromFFIType hs_bindgen_702437d53bf7e32f_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 3:6@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
foo ::
     BOOL
  -> IO ()
foo = hs_bindgen_702437d53bf7e32f

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_432c6480e8c1355b" hs_bindgen_432c6480e8c1355b_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_bar@
hs_bindgen_432c6480e8c1355b ::
     RIP.CInt
  -> IO ()
hs_bindgen_432c6480e8c1355b =
  RIP.fromFFIType hs_bindgen_432c6480e8c1355b_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 6:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
bar ::
     RIP.CInt
  -> IO ()
bar = hs_bindgen_432c6480e8c1355b

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_dash@
foreign import ccall safe "hs_bindgen_b7cd534964204668" hs_bindgen_b7cd534964204668_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_dash@
hs_bindgen_b7cd534964204668 ::
     RIP.CInt
  -> IO ()
hs_bindgen_b7cd534964204668 =
  RIP.fromFFIType hs_bindgen_b7cd534964204668_base

{-| __C declaration:__ @dash@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 7:46@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
dash ::
     RIP.CInt
  -> IO ()
dash = hs_bindgen_b7cd534964204668

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_quux@
foreign import ccall safe "hs_bindgen_24c242811e7f290b" hs_bindgen_24c242811e7f290b_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_quux@
hs_bindgen_24c242811e7f290b ::
     RIP.CInt
  -> IO ()
hs_bindgen_24c242811e7f290b =
  RIP.fromFFIType hs_bindgen_24c242811e7f290b_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 11:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
quux ::
     RIP.CInt
  -> IO ()
quux = hs_bindgen_24c242811e7f290b

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_heq@
foreign import ccall safe "hs_bindgen_9bea1bb33c269bc2" hs_bindgen_9bea1bb33c269bc2_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosreparsegnu_attributes_Example_Safe_heq@
hs_bindgen_9bea1bb33c269bc2 ::
     RIP.CInt
  -> IO ()
hs_bindgen_9bea1bb33c269bc2 =
  RIP.fromFFIType hs_bindgen_9bea1bb33c269bc2_base

{-| __C declaration:__ @heq@

    __defined at:__ @macros\/reparse\/gnu_attributes.h 12:13@

    __exported by:__ @macros\/reparse\/gnu_attributes.h@
-}
heq ::
     RIP.CInt
  -> IO ()
heq = hs_bindgen_9bea1bb33c269bc2
