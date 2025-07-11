{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Block
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (IO)

$(CAPI.addCSource "#include \"iterator.h\"\nToggle testmodule_makeToggle (_Bool arg1) { return makeToggle(arg1); }\n_Bool testmodule_toggleNext (Toggle arg1) { return toggleNext(arg1); }\nvoid testmodule_releaseToggle (Toggle arg1) { releaseToggle(arg1); }\nCounter testmodule_makeCounter (signed int arg1, signed int arg2) { return makeCounter(arg1, arg2); }\nsigned int testmodule_counterNext (Counter arg1) { return counterNext(arg1); }\nvoid testmodule_releaseCounter (Counter arg1) { releaseCounter(arg1); }\nVarCounter testmodule_makeVarCounter (signed int arg1) { return makeVarCounter(arg1); }\nsigned int testmodule_varCounterNext (VarCounter arg1, signed int arg2) { return varCounterNext(arg1, arg2); }\nvoid testmodule_releaseVarCounter (VarCounter arg1) { releaseVarCounter(arg1); }\n")

newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

foreign import ccall safe "testmodule_makeToggle" makeToggle :: FC.CBool -> IO Toggle

foreign import ccall safe "testmodule_toggleNext" toggleNext :: Toggle -> IO FC.CBool

foreign import ccall safe "testmodule_releaseToggle" releaseToggle :: Toggle -> IO ()

newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

foreign import ccall safe "testmodule_makeCounter" makeCounter :: FC.CInt -> FC.CInt -> IO Counter

foreign import ccall safe "testmodule_counterNext" counterNext :: Counter -> IO FC.CInt

foreign import ccall safe "testmodule_releaseCounter" releaseCounter :: Counter -> IO ()

newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

foreign import ccall safe "testmodule_makeVarCounter" makeVarCounter :: FC.CInt -> IO VarCounter

foreign import ccall safe "testmodule_varCounterNext" varCounterNext :: VarCounter -> FC.CInt -> IO FC.CInt

foreign import ccall safe "testmodule_releaseVarCounter" releaseVarCounter :: VarCounter -> IO ()
