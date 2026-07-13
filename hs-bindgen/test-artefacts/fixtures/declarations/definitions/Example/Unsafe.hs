{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_07fd5b433f381094 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return (foo)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_07fd5b433f381094" hs_bindgen_07fd5b433f381094_base ::
     Double
  -> IO BG.Int32

-- __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
hs_bindgen_07fd5b433f381094 ::
     BG.CDouble
  -> IO BG.CInt
hs_bindgen_07fd5b433f381094 =
  BG.fromFFIType hs_bindgen_07fd5b433f381094_base

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> IO BG.CInt
foo = hs_bindgen_07fd5b433f381094
