{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <declarations/declarations_required_for_scoping.h>"
  , "void hs_bindgen_0d1c75136a36e326 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  f(arg1);"
  , "}"
  ]))

-- __unique:__ @test_declarationsdeclarations_requ_Example_Safe_f@
foreign import ccall safe "hs_bindgen_0d1c75136a36e326" hs_bindgen_0d1c75136a36e326_base ::
     FC.CSize
  -> IO ()

-- __unique:__ @test_declarationsdeclarations_requ_Example_Safe_f@
hs_bindgen_0d1c75136a36e326 ::
     A
  -> IO ()
hs_bindgen_0d1c75136a36e326 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_0d1c75136a36e326_base

{-| __C declaration:__ @f@

    __defined at:__ @declarations\/declarations_required_for_scoping.h 7:6@

    __exported by:__ @declarations\/declarations_required_for_scoping.h@
-}
f ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_0d1c75136a36e326
