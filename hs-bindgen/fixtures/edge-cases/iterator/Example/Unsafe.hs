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
  [ "#include <edge-cases/iterator.h>"
  , "Toggle hs_bindgen_test_edgecasesiterator_e08915debbe98c63 ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_test_edgecasesiterator_4856a1e6d5899f7a ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_17fdda1fbbe08ed6 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_test_edgecasesiterator_2a87d5fbd078f0cd ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_12d5686d1b44e395 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_17d84f29376b523f ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_test_edgecasesiterator_c08ad8a2cfcaadb9 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_test_edgecasesiterator_47fb76b56573601b ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_edgecasesiterator_e7db0fed62ae8e63 ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_makeToggle@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_e08915debbe98c63" makeToggle ::
     FC.CBool
     {- ^ __C declaration:__ @start@
     -}
  -> IO Toggle

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_toggleNext@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_4856a1e6d5899f7a" toggleNext ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CBool

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_releaseToggle@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_17fdda1fbbe08ed6" releaseToggle ::
     Toggle
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_makeCounter@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_2a87d5fbd078f0cd" makeCounter ::
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

    __unique:__ @Example_Unsafe_counterNext@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_12d5686d1b44e395" counterNext ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO FC.CInt

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_releaseCounter@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_17d84f29376b523f" releaseCounter ::
     Counter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_makeVarCounter@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_c08ad8a2cfcaadb9" makeVarCounter ::
     FC.CInt
     {- ^ __C declaration:__ @start@
     -}
  -> IO VarCounter

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @Example_Unsafe_varCounterNext@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_47fb76b56573601b" varCounterNext ::
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

    __unique:__ @Example_Unsafe_releaseVarCounter@
-}
foreign import ccall unsafe "hs_bindgen_test_edgecasesiterator_e7db0fed62ae8e63" releaseVarCounter ::
     VarCounter
     {- ^ __C declaration:__ @block@
     -}
  -> IO ()
