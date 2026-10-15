{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_1cabb32c661d9a0e ::
     A_Aux
  -> IO (BG.FunPtr A_Aux)

hs_bindgen_cdb12400c6863f15 ::
     BG.FunPtr A_Aux
  -> A_Aux
