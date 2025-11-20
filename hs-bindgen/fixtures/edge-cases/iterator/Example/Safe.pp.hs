{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Marshallable
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

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_900530c0457bf5ee" makeToggle_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CBool
    -> IO Toggle
    )

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle ::
     FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle
makeToggle =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType makeToggle_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_0584fdb88b39872d" toggleNext_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Toggle
    -> IO FC.CBool
    )

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool
toggleNext =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType toggleNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_6d013ab8b38fc1d9" releaseToggle_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Toggle
    -> IO ()
    )

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
releaseToggle =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType releaseToggle_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_10d749b6b17037ba" makeCounter_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> FC.CInt
    -> IO Counter
    )

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO Counter
makeCounter =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType makeCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_9695aacc59a66573" counterNext_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Counter
    -> IO FC.CInt
    )

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt
counterNext =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType counterNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_49a436aa15c7ea70" releaseCounter_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Counter
    -> IO ()
    )

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
releaseCounter =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType releaseCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_f12a50c4468834b5" makeVarCounter_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> IO VarCounter
    )

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter
makeVarCounter =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType makeVarCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_2c8a2667de9b1ffb" varCounterNext_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       VarCounter
    -> FC.CInt
    -> IO FC.CInt
    )

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO FC.CInt
varCounterNext =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType varCounterNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_edgecasesiterator_023ae1f9abf46556" releaseVarCounter_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       VarCounter
    -> IO ()
    )

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
releaseVarCounter =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType releaseVarCounter_base
