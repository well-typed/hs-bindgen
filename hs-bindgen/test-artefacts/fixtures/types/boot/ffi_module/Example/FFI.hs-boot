{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_70c2110bb36ed51a ::
     On_event_Aux
  -> IO (BG.FunPtr On_event_Aux)

hs_bindgen_a7705d093ccaa37a ::
     BG.FunPtr On_event_Aux
  -> On_event_Aux

hs_bindgen_ce6081d63db40b52 ::
     (BG.Ptr Point -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr Point -> IO BG.CInt))

hs_bindgen_9ee31aa034cd5991 ::
     BG.FunPtr (BG.Ptr Point -> IO BG.CInt)
  -> BG.Ptr Point -> IO BG.CInt
