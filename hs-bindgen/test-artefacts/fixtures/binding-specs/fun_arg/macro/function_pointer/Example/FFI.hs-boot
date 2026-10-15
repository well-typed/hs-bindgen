{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_47dfd04698dd2e6f ::
     MyFunctionPointer_Aux
  -> IO (BG.FunPtr MyFunctionPointer_Aux)

hs_bindgen_5738272f94a589e2 ::
     BG.FunPtr MyFunctionPointer_Aux
  -> MyFunctionPointer_Aux
