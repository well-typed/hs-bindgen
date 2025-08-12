{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include <iterator.h>\nToggle hs_bindgen_test_iterator_4f34fce61cc68c9f (_Bool arg1) { return makeToggle(arg1); }\n_Bool hs_bindgen_test_iterator_bfb4e32e3a824c7e (Toggle arg1) { return toggleNext(arg1); }\nvoid hs_bindgen_test_iterator_8d23fba933ba9584 (Toggle arg1) { releaseToggle(arg1); }\nCounter hs_bindgen_test_iterator_5b455070cb6127b9 (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }\nsigned int hs_bindgen_test_iterator_1eb9473844c466c6 (Counter arg1) { return counterNext(arg1); }\nvoid hs_bindgen_test_iterator_4bd3562b992f2f1c (Counter arg1) { releaseCounter(arg1); }\nVarCounter hs_bindgen_test_iterator_0fc005ef62990438 (signed int arg1) { return makeVarCounter(arg1); }\nsigned int hs_bindgen_test_iterator_a88cd5c9559b5d52 (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }\nvoid hs_bindgen_test_iterator_2d2d26e60eea04a8 (VarCounter arg1) { releaseVarCounter(arg1); }\n")

newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

foreign import ccall safe "hs_bindgen_test_iterator_4f34fce61cc68c9f" makeToggle :: FC.CBool -> IO Toggle

foreign import ccall safe "hs_bindgen_test_iterator_bfb4e32e3a824c7e" toggleNext :: Toggle -> IO FC.CBool

foreign import ccall safe "hs_bindgen_test_iterator_8d23fba933ba9584" releaseToggle :: Toggle -> IO ()

newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

foreign import ccall safe "hs_bindgen_test_iterator_5b455070cb6127b9" makeCounter :: FC.CInt -> FC.CInt -> IO Counter

foreign import ccall safe "hs_bindgen_test_iterator_1eb9473844c466c6" counterNext :: Counter -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_iterator_4bd3562b992f2f1c" releaseCounter :: Counter -> IO ()

newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

foreign import ccall safe "hs_bindgen_test_iterator_0fc005ef62990438" makeVarCounter :: FC.CInt -> IO VarCounter

foreign import ccall safe "hs_bindgen_test_iterator_a88cd5c9559b5d52" varCounterNext :: VarCounter -> FC.CInt -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_iterator_2d2d26e60eea04a8" releaseVarCounter :: VarCounter -> IO ()
