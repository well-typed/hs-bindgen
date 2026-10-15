{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (BG.FunPtr RunDriver_Aux)

hs_bindgen_6520ae39b50ffb4e ::
     BG.FunPtr RunDriver_Aux
  -> RunDriver_Aux
