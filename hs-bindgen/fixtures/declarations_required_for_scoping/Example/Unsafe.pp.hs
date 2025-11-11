{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations_required_for_scoping.h>"
  , "void hs_bindgen_test_declarations_required_for_scop_b68b2cffacc97c1d ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @f@

    __defined at:__ @declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations_required_for_scoping.h@
-}
foreign import ccall unsafe "hs_bindgen_test_declarations_required_for_scop_b68b2cffacc97c1d" f ::
     A
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
