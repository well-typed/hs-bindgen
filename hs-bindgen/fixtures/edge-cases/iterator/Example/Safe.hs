{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/iterator.h>"
  , "Toggle hs_bindgen_test_edgecasesiterator_cb96749fe636b720 ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_edgecasesiterator_ccf2cd9b083796e3 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_589ea1e89f3f5013 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_test_edgecasesiterator_f44b4a93b25713e5 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_43a100bdb60a8b48 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_6090b71ef3b037ba ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_test_edgecasesiterator_1a06a7f41e69da61 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_2ab139260560251d ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_84ecbbfc5ebaf00d ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_makeToggle@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_cb96749fe636b720" makeToggle ::
     FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_toggleNext@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_ccf2cd9b083796e3" toggleNext ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_releaseToggle@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_589ea1e89f3f5013" releaseToggle ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_makeCounter@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_f44b4a93b25713e5" makeCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO Counter

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_counterNext@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_43a100bdb60a8b48" counterNext ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_releaseCounter@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_6090b71ef3b037ba" releaseCounter ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_makeVarCounter@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_1a06a7f41e69da61" makeVarCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_varCounterNext@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_2ab139260560251d" varCounterNext ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Safe_releaseVarCounter@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_84ecbbfc5ebaf00d" releaseVarCounter ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
