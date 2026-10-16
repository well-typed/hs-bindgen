{-# LANGUAGE NoImplicitPrelude #-}

module Example.HsBootInternal where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_70c2110bb36ed51a ::
     On_event_Aux
  -> IO (BG.FunPtr On_event_Aux)

hs_bindgen_a7705d093ccaa37a ::
     BG.FunPtr On_event_Aux
  -> On_event_Aux
