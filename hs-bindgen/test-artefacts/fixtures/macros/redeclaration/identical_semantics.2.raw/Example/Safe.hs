{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/redeclaration/identical_semantics.h>"
  , "void hs_bindgen_3a672aa51059499f ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_f6ce0c67437e90e7 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3a672aa51059499f" hs_bindgen_3a672aa51059499f_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
hs_bindgen_3a672aa51059499f ::
     BG.CInt
  -> IO ()
hs_bindgen_3a672aa51059499f =
  BG.fromFFIType hs_bindgen_3a672aa51059499f_base

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 3:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
foo ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3a672aa51059499f

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_f6ce0c67437e90e7" hs_bindgen_f6ce0c67437e90e7_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
hs_bindgen_f6ce0c67437e90e7 ::
     BG.CInt
  -> IO ()
hs_bindgen_f6ce0c67437e90e7 =
  BG.fromFFIType hs_bindgen_f6ce0c67437e90e7_base

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 5:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
bar ::
     BG.CInt
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_f6ce0c67437e90e7
