{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_b5a7b5e83ffee6b4 ::
     Foo_Aux
  -> IO (BG.FunPtr Foo_Aux)

hs_bindgen_223d08172bb37c01 ::
     BG.FunPtr Foo_Aux
  -> Foo_Aux
