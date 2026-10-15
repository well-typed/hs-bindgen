{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_4e8a459829b269e0 ::
     MyFunction
  -> IO (BG.FunPtr MyFunction)

hs_bindgen_bb71f7e730356103 ::
     BG.FunPtr MyFunction
  -> MyFunction
