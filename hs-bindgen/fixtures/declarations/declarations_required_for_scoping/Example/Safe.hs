{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "void hs_bindgen_test_declarationsdeclarations_requ_59d88239da8fef6b ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@

    __unique:__ @Example_Safe_f@
-}
foreign import ccall safe "hs_bindgen_test_declarationsdeclarations_requ_59d88239da8fef6b" f ::
     A
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
