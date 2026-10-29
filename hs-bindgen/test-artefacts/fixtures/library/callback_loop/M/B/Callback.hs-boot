{-# LANGUAGE NoImplicitPrelude #-}

module M.B.Callback where

import qualified HsBindgen.Runtime.Support as BG
import {-# SOURCE #-} qualified M.A
import Prelude (IO)
import {-# SOURCE #-} M.B

hs_bindgen_507c1de5873f8883 ::
     (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
  -> IO (BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()))

hs_bindgen_735802aaf5f6880e ::
     BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
  -> BG.Ptr B -> BG.Ptr M.A.A -> IO ()
