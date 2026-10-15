{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_b8534912f6256492 ::
     T
  -> IO (BG.FunPtr T)

hs_bindgen_8f830eadb43a6fe4 ::
     BG.FunPtr T
  -> T
