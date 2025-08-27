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

{-| __from C:__ @makeToggle@ -}
foreign import ccall safe "hs_bindgen_test_iterator_4f34fce61cc68c9f" makeToggle
  :: FC.CBool
     {- ^ __from C:__ @start@ -}
  -> IO Toggle

{-| __from C:__ @toggleNext@ -}
foreign import ccall safe "hs_bindgen_test_iterator_bfb4e32e3a824c7e" toggleNext
  :: Toggle
     {- ^ __from C:__ @block@ -}
  -> IO FC.CBool

{-| __from C:__ @releaseToggle@ -}
foreign import ccall safe "hs_bindgen_test_iterator_8d23fba933ba9584" releaseToggle
  :: Toggle
     {- ^ __from C:__ @block@ -}
  -> IO ()

newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

{-| __from C:__ @makeCounter@ -}
foreign import ccall safe "hs_bindgen_test_iterator_5b455070cb6127b9" makeCounter
  :: FC.CInt
     {- ^ __from C:__ @start@ -}
  -> FC.CInt
     {- ^ __from C:__ @increment@ -}
  -> IO Counter

{-| __from C:__ @counterNext@ -}
foreign import ccall safe "hs_bindgen_test_iterator_1eb9473844c466c6" counterNext
  :: Counter
     {- ^ __from C:__ @block@ -}
  -> IO FC.CInt

{-| __from C:__ @releaseCounter@ -}
foreign import ccall safe "hs_bindgen_test_iterator_4bd3562b992f2f1c" releaseCounter
  :: Counter
     {- ^ __from C:__ @block@ -}
  -> IO ()

newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

{-| __from C:__ @makeVarCounter@ -}
foreign import ccall safe "hs_bindgen_test_iterator_0fc005ef62990438" makeVarCounter
  :: FC.CInt
     {- ^ __from C:__ @start@ -}
  -> IO VarCounter

{-| __from C:__ @varCounterNext@ -}
foreign import ccall safe "hs_bindgen_test_iterator_a88cd5c9559b5d52" varCounterNext
  :: VarCounter
     {- ^ __from C:__ @block@ -}
  -> FC.CInt
     {- ^ __from C:__ @increment@ -}
  -> IO FC.CInt

{-| __from C:__ @releaseVarCounter@ -}
foreign import ccall safe "hs_bindgen_test_iterator_2d2d26e60eea04a8" releaseVarCounter
  :: VarCounter
     {- ^ __from C:__ @block@ -}
  -> IO ()
