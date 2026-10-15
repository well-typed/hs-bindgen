{-# LANGUAGE NoImplicitPrelude #-}

module Example.FFI where

import qualified HsBindgen.Runtime.Support as BG
import Prelude (IO)
import {-# SOURCE #-} Example

hs_bindgen_03e9923d8d301cca ::
     (BG.CShort -> IO I)
  -> IO (BG.FunPtr (BG.CShort -> IO I))

hs_bindgen_ee10a61bcb1a5d39 ::
     BG.FunPtr (BG.CShort -> IO I)
  -> BG.CShort -> IO I

hs_bindgen_140ec1313a27c405 ::
     (S -> IO BG.CInt)
  -> IO (BG.FunPtr (S -> IO BG.CInt))

hs_bindgen_e86a5fa3f995e6fb ::
     BG.FunPtr (S -> IO BG.CInt)
  -> S -> IO BG.CInt
