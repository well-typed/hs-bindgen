{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Shape.Callback where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Lib.Shape

hs_bindgen_e0535817c449f9e4 ::
     On_draw_Aux
  -> IO (BG.FunPtr On_draw_Aux)

hs_bindgen_7a6697890840e122 ::
     BG.FunPtr On_draw_Aux
  -> On_draw_Aux
