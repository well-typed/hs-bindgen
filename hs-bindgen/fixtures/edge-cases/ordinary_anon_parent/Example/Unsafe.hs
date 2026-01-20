{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/ordinary_anon_parent.h>"
  , "double hs_bindgen_dca60678b5047ee4 ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return _acos(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Unsafe__acos@
foreign import ccall unsafe "hs_bindgen_dca60678b5047ee4" hs_bindgen_dca60678b5047ee4_base ::
     FC.CDouble
  -> IO FC.CDouble

-- __unique:__ @test_edgecasesordinary_anon_paren_Example_Unsafe__acos@
hs_bindgen_dca60678b5047ee4 ::
     FC.CDouble
  -> IO FC.CDouble
hs_bindgen_dca60678b5047ee4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_dca60678b5047ee4_base

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h 4:1@

    __exported by:__ @edge-cases\/ordinary_anon_parent.h@
-}
_acos ::
     FC.CDouble
     -- ^ __C declaration:__ @x@
  -> IO FC.CDouble
_acos = hs_bindgen_dca60678b5047ee4
