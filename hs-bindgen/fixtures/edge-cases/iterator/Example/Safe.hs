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
  , "Toggle hs_bindgen_test_edgecasesiterator_900530c0457bf5ee ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_edgecasesiterator_0584fdb88b39872d ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_6d013ab8b38fc1d9 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_test_edgecasesiterator_10d749b6b17037ba ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_9695aacc59a66573 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_49a436aa15c7ea70 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_test_edgecasesiterator_f12a50c4468834b5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_2c8a2667de9b1ffb ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_023ae1f9abf46556 ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_900530c0457bf5ee" makeToggle ::
     FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_0584fdb88b39872d" toggleNext ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_6d013ab8b38fc1d9" releaseToggle ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_10d749b6b17037ba" makeCounter ::
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
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_9695aacc59a66573" counterNext ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_49a436aa15c7ea70" releaseCounter ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_f12a50c4468834b5" makeVarCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_2c8a2667de9b1ffb" varCounterNext ::
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
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_023ae1f9abf46556" releaseVarCounter ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
