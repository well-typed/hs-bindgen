{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_0c7d4776a632d026 ::
     A
  -> IO (BG.FunPtr A)

hs_bindgen_0cf9a6d50f563441 ::
     BG.FunPtr A
  -> A
