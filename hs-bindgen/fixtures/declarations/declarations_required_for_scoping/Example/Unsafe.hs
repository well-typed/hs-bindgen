{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "void hs_bindgen_test_declarationsdeclarations_requ_bae09f29e7f99a8f ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h:7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@

    __unique:__ @Example_Unsafe_f@
-}
foreign import ccall unsafe "hs_bindgen_test_declarationsdeclarations_requ_bae09f29e7f99a8f" f ::
     A
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
