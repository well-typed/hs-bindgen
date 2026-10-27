{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module M.A.Callback where

import qualified HsBindgen.Runtime.Support as BG
import qualified M.B
import M.A
import Prelude (IO, fmap)

-- __unique:__ @instance ToFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())@
foreign import ccall safe "wrapper" hs_bindgen_c6d383e1471a9625_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()))

-- __unique:__ @instance ToFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())@
hs_bindgen_c6d383e1471a9625 ::
     (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
  -> IO (BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ()))
hs_bindgen_c6d383e1471a9625 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_c6d383e1471a9625_base (\x1 ->
                                                            \x2 ->
                                                              fun0 (BG.fromFFIType x1) (BG.fromFFIType x2)))

-- __unique:__ @instance FromFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())@
foreign import ccall safe "dynamic" hs_bindgen_4f4c392c56000856_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ())
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO ()

-- __unique:__ @instance FromFunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())@
hs_bindgen_4f4c392c56000856 ::
     BG.FunPtr (BG.Ptr A -> BG.Ptr M.B.B -> IO ())
  -> BG.Ptr A -> BG.Ptr M.B.B -> IO ()
hs_bindgen_4f4c392c56000856 =
  \funPtr0 ->
    \x1 ->
      \x2 ->
        hs_bindgen_4f4c392c56000856_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)
