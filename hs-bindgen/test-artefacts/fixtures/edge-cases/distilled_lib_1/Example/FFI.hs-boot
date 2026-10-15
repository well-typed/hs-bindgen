{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_b6b6922e35047658 ::
     Callback_t_Aux
  -> IO (BG.FunPtr Callback_t_Aux)

hs_bindgen_d6debb4b8d5bb869 ::
     BG.FunPtr Callback_t_Aux
  -> Callback_t_Aux
