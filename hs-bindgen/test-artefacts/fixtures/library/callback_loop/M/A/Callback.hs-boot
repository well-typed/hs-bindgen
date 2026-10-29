{-# LANGUAGE NoImplicitPrelude #-}

module M.A.Callback where

import qualified HsBindgen.Runtime.Support as BG
import {-# SOURCE #-} qualified M.B
import Prelude (IO)
import {-# SOURCE #-} M.A

hs_bindgen_c6d383e1471a9625 ::
     (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
  -> IO (BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()))

hs_bindgen_4f4c392c56000856 ::
     BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
  -> BG.Ptr A -> BG.Ptr M.B.B -> IO ()
