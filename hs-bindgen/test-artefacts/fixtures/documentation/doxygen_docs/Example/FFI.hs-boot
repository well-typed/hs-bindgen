{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_111918b0aee2a7fb ::
     Event_callback_t_Aux
  -> IO (BG.FunPtr Event_callback_t_Aux)

hs_bindgen_9e9d478c2d75628c ::
     BG.FunPtr Event_callback_t_Aux
  -> Event_callback_t_Aux

hs_bindgen_d4e16471c82d5df0 ::
     Processor_fn_t_Aux
  -> IO (BG.FunPtr Processor_fn_t_Aux)

hs_bindgen_0d4b3d0461629423 ::
     BG.FunPtr Processor_fn_t_Aux
  -> Processor_fn_t_Aux
