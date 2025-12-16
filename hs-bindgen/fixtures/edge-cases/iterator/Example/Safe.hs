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
  , "Toggle hs_bindgen_9d01035006b66206 ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_ccd3ba727d0c0cf4 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_602b40e971b06c72 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_234fa6f1fb089e1d ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_f0fca62d78f225c3 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_e42dcbee8a114957 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_2bee4eb5b4d895c1 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_276b9cb5320fec37 ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_8423b076f7c9df21 ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

-- | __unique:__ @test_edgecasesiterator_Example_Safe_makeToggle@
foreign import ccall safe "hs_bindgen_9d01035006b66206" hs_bindgen_9d01035006b66206 ::
     FC.CBool
  -> IO Toggle

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle ::
     FC.CBool
     -- ^ __C declaration:__ @start@
  -> IO Toggle
makeToggle = hs_bindgen_9d01035006b66206

-- | __unique:__ @test_edgecasesiterator_Example_Safe_toggleNext@
foreign import ccall safe "hs_bindgen_ccd3ba727d0c0cf4" hs_bindgen_ccd3ba727d0c0cf4 ::
     Toggle
  -> IO FC.CBool

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO FC.CBool
toggleNext = hs_bindgen_ccd3ba727d0c0cf4

-- | __unique:__ @test_edgecasesiterator_Example_Safe_releaseToggle@
foreign import ccall safe "hs_bindgen_602b40e971b06c72" hs_bindgen_602b40e971b06c72 ::
     Toggle
  -> IO ()

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseToggle = hs_bindgen_602b40e971b06c72

-- | __unique:__ @test_edgecasesiterator_Example_Safe_makeCounter@
foreign import ccall safe "hs_bindgen_234fa6f1fb089e1d" hs_bindgen_234fa6f1fb089e1d ::
     FC.CInt
  -> FC.CInt
  -> IO Counter

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO Counter
makeCounter = hs_bindgen_234fa6f1fb089e1d

-- | __unique:__ @test_edgecasesiterator_Example_Safe_counterNext@
foreign import ccall safe "hs_bindgen_f0fca62d78f225c3" hs_bindgen_f0fca62d78f225c3 ::
     Counter
  -> IO FC.CInt

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO FC.CInt
counterNext = hs_bindgen_f0fca62d78f225c3

-- | __unique:__ @test_edgecasesiterator_Example_Safe_releaseCounter@
foreign import ccall safe "hs_bindgen_e42dcbee8a114957" hs_bindgen_e42dcbee8a114957 ::
     Counter
  -> IO ()

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseCounter = hs_bindgen_e42dcbee8a114957

-- | __unique:__ @test_edgecasesiterator_Example_Safe_makeVarCounter@
foreign import ccall safe "hs_bindgen_2bee4eb5b4d895c1" hs_bindgen_2bee4eb5b4d895c1 ::
     FC.CInt
  -> IO VarCounter

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> IO VarCounter
makeVarCounter = hs_bindgen_2bee4eb5b4d895c1

-- | __unique:__ @test_edgecasesiterator_Example_Safe_varCounterNext@
foreign import ccall safe "hs_bindgen_276b9cb5320fec37" hs_bindgen_276b9cb5320fec37 ::
     VarCounter
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO FC.CInt
varCounterNext = hs_bindgen_276b9cb5320fec37

-- | __unique:__ @test_edgecasesiterator_Example_Safe_releaseVarCounter@
foreign import ccall safe "hs_bindgen_8423b076f7c9df21" hs_bindgen_8423b076f7c9df21 ::
     VarCounter
  -> IO ()

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseVarCounter = hs_bindgen_8423b076f7c9df21
