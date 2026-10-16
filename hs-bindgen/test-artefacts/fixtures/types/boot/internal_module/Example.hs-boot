{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Support as BG

data On_event_Aux

data On_event

data Buffer_Aux

type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux
