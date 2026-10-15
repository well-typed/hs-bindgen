{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_00d16e666202ed6c ::
     F1_Aux
  -> IO (BG.FunPtr F1_Aux)

hs_bindgen_ddeb5206e8192425 ::
     BG.FunPtr F1_Aux
  -> F1_Aux

hs_bindgen_c39d7524b75b54e8 ::
     F2_Aux
  -> IO (BG.FunPtr F2_Aux)

hs_bindgen_e15bcd26f1ed1df7 ::
     BG.FunPtr F2_Aux
  -> F2_Aux

hs_bindgen_4a960721e7d1dcef ::
     F3_Aux
  -> IO (BG.FunPtr F3_Aux)

hs_bindgen_66460422a7197535 ::
     BG.FunPtr F3_Aux
  -> F3_Aux

hs_bindgen_83bcff023b3bc648 ::
     F4_Aux
  -> IO (BG.FunPtr F4_Aux)

hs_bindgen_40f9a8d432b9eb97 ::
     BG.FunPtr F4_Aux
  -> F4_Aux

hs_bindgen_6891cbd81d6f42b9 ::
     F5_Aux
  -> IO (BG.FunPtr F5_Aux)

hs_bindgen_586f6635c057975f ::
     BG.FunPtr F5_Aux
  -> F5_Aux

hs_bindgen_c1baf73f98614f45 ::
     F6_Aux
  -> IO (BG.FunPtr F6_Aux)

hs_bindgen_a887947b26e58f0c ::
     BG.FunPtr F6_Aux
  -> F6_Aux
