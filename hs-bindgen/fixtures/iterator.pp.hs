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

{-| __/Automatically generated from C/__

    __C declaration:__ @Toggle@

    __defined at:__ @iterator.h:3:16@

    __exported by:__ @iterator.h@
-}
newtype Toggle = Toggle
  { un_Toggle :: HsBindgen.Runtime.Block.Block (IO FC.CBool)
  }

{-| __/Automatically generated from C/__

    __C declaration:__ @makeToggle@

    __defined at:__ @iterator.h:4:8@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_4f34fce61cc68c9f" makeToggle
  :: FC.CBool
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __/Automatically generated from C/__

    __C declaration:__ @toggleNext@

    __defined at:__ @iterator.h:5:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_bfb4e32e3a824c7e" toggleNext
  :: Toggle
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __/Automatically generated from C/__

    __C declaration:__ @releaseToggle@

    __defined at:__ @iterator.h:6:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_8d23fba933ba9584" releaseToggle
  :: Toggle
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @Counter@

    __defined at:__ @iterator.h:10:14@

    __exported by:__ @iterator.h@
-}
newtype Counter = Counter
  { un_Counter :: HsBindgen.Runtime.Block.Block (IO FC.CInt)
  }

{-| __/Automatically generated from C/__

    __C declaration:__ @makeCounter@

    __defined at:__ @iterator.h:11:9@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_5b455070cb6127b9" makeCounter
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @start@
     -}
  -> FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @increment@
     -}
  -> IO Counter

{-| __/Automatically generated from C/__

    __C declaration:__ @counterNext@

    __defined at:__ @iterator.h:12:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_1eb9473844c466c6" counterNext
  :: Counter
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __/Automatically generated from C/__

    __C declaration:__ @releaseCounter@

    __defined at:__ @iterator.h:13:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_4bd3562b992f2f1c" releaseCounter
  :: Counter
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> IO ()

{-| __/Automatically generated from C/__

    __C declaration:__ @VarCounter@

    __defined at:__ @iterator.h:17:14@

    __exported by:__ @iterator.h@
-}
newtype VarCounter = VarCounter
  { un_VarCounter :: HsBindgen.Runtime.Block.Block (FC.CInt -> IO FC.CInt)
  }

{-| __/Automatically generated from C/__

    __C declaration:__ @makeVarCounter@

    __defined at:__ @iterator.h:18:12@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_0fc005ef62990438" makeVarCounter
  :: FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __/Automatically generated from C/__

    __C declaration:__ @varCounterNext@

    __defined at:__ @iterator.h:19:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_a88cd5c9559b5d52" varCounterNext
  :: VarCounter
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> FC.CInt
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @increment@
     -}
  -> IO FC.CInt

{-| __/Automatically generated from C/__

    __C declaration:__ @releaseVarCounter@

    __defined at:__ @iterator.h:20:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_iterator_2d2d26e60eea04a8" releaseVarCounter
  :: VarCounter
     {- ^ __/Automatically generated from C/__

          __C declaration:__ @block@
     -}
  -> IO ()
