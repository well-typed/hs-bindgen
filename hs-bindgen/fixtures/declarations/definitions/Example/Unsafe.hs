{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_07fd5b433f381094 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_07fd5b433f381094" hs_bindgen_07fd5b433f381094_base ::
     Double
  -> IO RIP.Int32

-- __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
hs_bindgen_07fd5b433f381094 ::
     RIP.CDouble
  -> IO RIP.CInt
hs_bindgen_07fd5b433f381094 =
  RIP.fromFFIType hs_bindgen_07fd5b433f381094_base

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> IO RIP.CInt
foo = hs_bindgen_07fd5b433f381094
