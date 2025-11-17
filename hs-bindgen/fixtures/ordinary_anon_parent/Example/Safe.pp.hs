{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <ordinary_anon_parent.h>"
  , "double hs_bindgen_test_ordinary_anon_parent_49d2dc050c5f231a ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return _acos(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @_acos@

    __defined at:__ @ordinary_anon_child.h:4:1@

    __exported by:__ @ordinary_anon_parent.h@
-}
foreign import ccall safe "hs_bindgen_test_ordinary_anon_parent_49d2dc050c5f231a" _acos ::
     FC.CDouble
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CDouble
