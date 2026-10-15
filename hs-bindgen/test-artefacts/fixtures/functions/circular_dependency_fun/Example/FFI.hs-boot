{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_5964bbadb359ee4a ::
     Fun_ptr_Aux
  -> IO (BG.FunPtr Fun_ptr_Aux)

hs_bindgen_f8391e85af67fcb6 ::
     BG.FunPtr Fun_ptr_Aux
  -> Fun_ptr_Aux
