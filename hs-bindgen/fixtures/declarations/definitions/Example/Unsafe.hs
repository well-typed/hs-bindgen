{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/definitions.h>"
  , "signed int hs_bindgen_07fd5b433f381094 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return foo(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @foo@

    __defined at:__ @declarations\/definitions.h:13:5@

    __exported by:__ @declarations\/definitions.h@

    __unique:__ @test_declarationsdefinitions_Example_Unsafe_foo@
-}
foreign import ccall unsafe "hs_bindgen_07fd5b433f381094" foo ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
