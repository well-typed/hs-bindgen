{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_5927fedc3abfa99c ::
     T_Aux
  -> IO (BG.FunPtr T_Aux)

hs_bindgen_281617c90fa9307a ::
     BG.FunPtr T_Aux
  -> T_Aux
