{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.bar
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/redeclaration/identical_semantics.h>"
  , "void hs_bindgen_3a672aa51059499f ("
  , "  T arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_f6ce0c67437e90e7 ("
  , "  T arg1"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_3a672aa51059499f" hs_bindgen_3a672aa51059499f_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_foo@
hs_bindgen_3a672aa51059499f ::
     T
  -> IO ()
hs_bindgen_3a672aa51059499f =
  \x0 ->
    hs_bindgen_3a672aa51059499f_base (BG.toFFIType x0)

{-| __C declaration:__ @foo@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 3:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
foo ::
     T
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_3a672aa51059499f

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_f6ce0c67437e90e7" hs_bindgen_f6ce0c67437e90e7_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosredeclarationidentical_Example_Safe_bar@
hs_bindgen_f6ce0c67437e90e7 ::
     T
  -> IO ()
hs_bindgen_f6ce0c67437e90e7 =
  \x0 ->
    hs_bindgen_f6ce0c67437e90e7_base (BG.toFFIType x0)

{-| __C declaration:__ @bar@

    __defined at:__ @macros\/redeclaration\/identical_semantics.h 5:6@

    __exported by:__ @macros\/redeclaration\/identical_semantics.h@
-}
bar ::
     T
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_f6ce0c67437e90e7
