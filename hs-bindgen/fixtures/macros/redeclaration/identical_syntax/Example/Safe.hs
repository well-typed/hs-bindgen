{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/redeclaration/identical_syntax.h>"
  , "void hs_bindgen_3a672aa51059499f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_f6ce0c67437e90e7 ("
  , "  char arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3a672aa51059499f" hs_bindgen_3a672aa51059499f_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
hs_bindgen_3a672aa51059499f ::
     RIP.CInt
  -> IO ()
hs_bindgen_3a672aa51059499f =
  RIP.fromFFIType hs_bindgen_3a672aa51059499f_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_syntax.h 6:6@

    __exported by:__ @macros\/redeclaration\/identical_syntax.h@
-}
foo ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3a672aa51059499f

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_f6ce0c67437e90e7" hs_bindgen_f6ce0c67437e90e7_base ::
     RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
hs_bindgen_f6ce0c67437e90e7 ::
     RIP.CChar
  -> IO ()
hs_bindgen_f6ce0c67437e90e7 =
  RIP.fromFFIType hs_bindgen_f6ce0c67437e90e7_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_syntax.h 10:6@

    __exported by:__ @macros\/redeclaration\/identical_syntax.h@
-}
bar ::
     RIP.CChar
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_f6ce0c67437e90e7
