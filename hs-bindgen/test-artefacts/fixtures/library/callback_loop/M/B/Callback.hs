{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module M.B.Callback where

import qualified HsBindgen.Runtime.Support as BG
import qualified M.A
import M.B
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_507c1de5873f8883_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())@
hs_bindgen_507c1de5873f8883 ::
     (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
  -> IO (BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ()))
hs_bindgen_507c1de5873f8883 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_507c1de5873f8883_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_735802aaf5f6880e_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())@
hs_bindgen_735802aaf5f6880e ::
     BG.FunPtr (BG.Ptr B -> BG.Ptr M.A.A -> IO ())
  -> BG.Ptr B -> BG.Ptr M.A.A -> IO ()
hs_bindgen_735802aaf5f6880e =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_735802aaf5f6880e_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)
