{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <iterator.h>"
  , "Toggle hs_bindgen_test_iterator_bd3974ddabfde9b8 ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_iterator_055df53eaf199b0e ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_test_iterator_07fb95f9614be94f ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_test_iterator_af5aabad780cc152 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_iterator_009eccead86c2acf ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_test_iterator_74f266597157c353 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_test_iterator_61a09dd9011981f5 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_test_iterator_4d944ebb7dc53d23 ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_iterator_502ca8978fd91294 ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @makeToggle@

    __defined at:__ @iterator.h:4:8@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_bd3974ddabfde9b8" makeToggle ::
     FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __C declaration:__ @toggleNext@

    __defined at:__ @iterator.h:5:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_055df53eaf199b0e" toggleNext ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @iterator.h:6:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_07fb95f9614be94f" releaseToggle ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeCounter@

    __defined at:__ @iterator.h:11:9@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_af5aabad780cc152" makeCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO Counter

{-| __C declaration:__ @counterNext@

    __defined at:__ @iterator.h:12:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_009eccead86c2acf" counterNext ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @iterator.h:13:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_74f266597157c353" releaseCounter ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @iterator.h:18:12@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_61a09dd9011981f5" makeVarCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @iterator.h:19:5@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_4d944ebb7dc53d23" varCounterNext ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> FC.CInt
     {- ^ __C declaration:__ @increment@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @iterator.h:20:6@

    __exported by:__ @iterator.h@
-}
foreign import ccall unsafe "hs_bindgen_test_iterator_502ca8978fd91294" releaseVarCounter ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
