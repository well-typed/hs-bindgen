{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_eca2bca8e63194be ::
     Toggle_Aux
  -> IO (BG.FunPtr Toggle_Aux)

hs_bindgen_703fc4bdc168721d ::
     BG.FunPtr Toggle_Aux
  -> Toggle_Aux

hs_bindgen_2202848aad97fe0a ::
     Counter_Aux
  -> IO (BG.FunPtr Counter_Aux)

hs_bindgen_73304cb84e9a2f8f ::
     BG.FunPtr Counter_Aux
  -> Counter_Aux

hs_bindgen_42a7337570f8b0d0 ::
     VarCounter_Aux
  -> IO (BG.FunPtr VarCounter_Aux)

hs_bindgen_43d902480175fccf ::
     BG.FunPtr VarCounter_Aux
  -> VarCounter_Aux
