{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Prelude (Double, IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
  -> IO GHC.Int.Int32

-- __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
hs_bindgen_07fd5b433f381094 ::
     FC.CDouble
  -> IO FC.CInt
hs_bindgen_07fd5b433f381094 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_07fd5b433f381094_base

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h 13:5@

    __exported by:__ @declarations\/definitions.h@
-}
foo ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> IO FC.CInt
foo = hs_bindgen_07fd5b433f381094
