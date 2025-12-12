{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
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

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_9d01035006b66206" makeToggle_base ::
     FC.CBool
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h:4:8@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_makeToggle@
-}
makeToggle ::
     FC.CBool
     -- ^ __C declaration:__ @start@
  -> IO Toggle
makeToggle =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType makeToggle_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_ccd3ba727d0c0cf4" toggleNext_base ::
     Ptr.Ptr Void
  -> IO FC.CBool

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h:5:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_toggleNext@
-}
toggleNext ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO FC.CBool
toggleNext =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType toggleNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_602b40e971b06c72" releaseToggle_base ::
     Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h:6:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_releaseToggle@
-}
releaseToggle ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseToggle =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType releaseToggle_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_234fa6f1fb089e1d" makeCounter_base ::
     FC.CInt
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h:11:9@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_makeCounter@
-}
makeCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO Counter
makeCounter =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType makeCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_f0fca62d78f225c3" counterNext_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h:12:5@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_counterNext@
-}
counterNext ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO FC.CInt
counterNext =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType counterNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_e42dcbee8a114957" releaseCounter_base ::
     Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h:13:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_releaseCounter@
-}
releaseCounter ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseCounter =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType releaseCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2bee4eb5b4d895c1" makeVarCounter_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h:18:12@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_makeVarCounter@
-}
makeVarCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> IO VarCounter
makeVarCounter =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType makeVarCounter_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_276b9cb5320fec37" varCounterNext_base ::
     Ptr.Ptr Void
  -> FC.CInt
  -> IO FC.CInt

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h:19:5@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_varCounterNext@
-}
varCounterNext ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO FC.CInt
varCounterNext =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType varCounterNext_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_8423b076f7c9df21" releaseVarCounter_base ::
     Ptr.Ptr Void
  -> IO ()

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h:20:6@

    __exported by:__ @edge-cases\/iterator.h@

    __unique:__ @test_edgecasesiterator_Example_Safe_releaseVarCounter@
-}
releaseVarCounter ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseVarCounter =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType releaseVarCounter_base
