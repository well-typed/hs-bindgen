{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified GHC.Word
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "void hs_bindgen_93ed1628a0edf6b0 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdeclarations_requ_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_93ed1628a0edf6b0" hs_bindgen_93ed1628a0edf6b0_base ::
     GHC.Word.Word64
  -> IO ()

-- __unique:__ @test_declarationsdeclarations_requ_Example_Unsafe_f@
hs_bindgen_93ed1628a0edf6b0 ::
     A
  -> IO ()
hs_bindgen_93ed1628a0edf6b0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_93ed1628a0edf6b0_base

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_93ed1628a0edf6b0
