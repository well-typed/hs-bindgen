{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Word
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <edge-cases/iterator.h>"
  , "Toggle hs_bindgen_1b7a6a61a9c0da07 ("
  , "  _Bool arg1"
  , ")"
  , "{"
  , "  return makeToggle(arg1);"
  , "}"
  , "_Bool hs_bindgen_4d2d650f2c8798d6 ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  return toggleNext(arg1);"
  , "}"
  , "void hs_bindgen_ddbe11e76502cbdc ("
  , "  Toggle arg1"
  , ")"
  , "{"
  , "  releaseToggle(arg1);"
  , "}"
  , "Counter hs_bindgen_2b04d558934551d2 ("
  , "  signed int arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return makeCounter(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_5bba69c8bfbeedf0 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  return counterNext(arg1);"
  , "}"
  , "void hs_bindgen_429845bb55a5a7b5 ("
  , "  Counter arg1"
  , ")"
  , "{"
  , "  releaseCounter(arg1);"
  , "}"
  , "VarCounter hs_bindgen_4421633e88fc96c4 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return makeVarCounter(arg1);"
  , "}"
  , "signed int hs_bindgen_31edd817cb78027d ("
  , "  VarCounter arg1,"
  , "  signed int arg2"
  , ")"
  , "{"
  , "  return varCounterNext(arg1, arg2);"
  , "}"
  , "void hs_bindgen_32e5b257124f69a2 ("
  , "  VarCounter arg1"
  , ")"
  , "{"
  , "  releaseVarCounter(arg1);"
  , "}"
  ]))

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeToggle@
foreign import ccall unsafe "hs_bindgen_1b7a6a61a9c0da07" hs_bindgen_1b7a6a61a9c0da07_base ::
     GHC.Word.Word8
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeToggle@
hs_bindgen_1b7a6a61a9c0da07 ::
     FC.CBool
  -> IO Toggle
hs_bindgen_1b7a6a61a9c0da07 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1b7a6a61a9c0da07_base

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h 4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle ::
     FC.CBool
     -- ^ __C declaration:__ @start@
  -> IO Toggle
makeToggle = hs_bindgen_1b7a6a61a9c0da07

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_toggleNext@
foreign import ccall unsafe "hs_bindgen_4d2d650f2c8798d6" hs_bindgen_4d2d650f2c8798d6_base ::
     Ptr.Ptr Void
  -> IO GHC.Word.Word8

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_toggleNext@
hs_bindgen_4d2d650f2c8798d6 ::
     Toggle
  -> IO FC.CBool
hs_bindgen_4d2d650f2c8798d6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4d2d650f2c8798d6_base

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h 5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO FC.CBool
toggleNext = hs_bindgen_4d2d650f2c8798d6

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseToggle@
foreign import ccall unsafe "hs_bindgen_ddbe11e76502cbdc" hs_bindgen_ddbe11e76502cbdc_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseToggle@
hs_bindgen_ddbe11e76502cbdc ::
     Toggle
  -> IO ()
hs_bindgen_ddbe11e76502cbdc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ddbe11e76502cbdc_base

{-| __C declaration:__ @releaseToggle@

    __defined at:__ @edge-cases\/iterator.h 6:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseToggle ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseToggle = hs_bindgen_ddbe11e76502cbdc

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeCounter@
foreign import ccall unsafe "hs_bindgen_2b04d558934551d2" hs_bindgen_2b04d558934551d2_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeCounter@
hs_bindgen_2b04d558934551d2 ::
     FC.CInt
  -> FC.CInt
  -> IO Counter
hs_bindgen_2b04d558934551d2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2b04d558934551d2_base

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h 11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO Counter
makeCounter = hs_bindgen_2b04d558934551d2

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_counterNext@
foreign import ccall unsafe "hs_bindgen_5bba69c8bfbeedf0" hs_bindgen_5bba69c8bfbeedf0_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_counterNext@
hs_bindgen_5bba69c8bfbeedf0 ::
     Counter
  -> IO FC.CInt
hs_bindgen_5bba69c8bfbeedf0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5bba69c8bfbeedf0_base

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h 12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO FC.CInt
counterNext = hs_bindgen_5bba69c8bfbeedf0

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseCounter@
foreign import ccall unsafe "hs_bindgen_429845bb55a5a7b5" hs_bindgen_429845bb55a5a7b5_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseCounter@
hs_bindgen_429845bb55a5a7b5 ::
     Counter
  -> IO ()
hs_bindgen_429845bb55a5a7b5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_429845bb55a5a7b5_base

{-| __C declaration:__ @releaseCounter@

    __defined at:__ @edge-cases\/iterator.h 13:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseCounter ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseCounter = hs_bindgen_429845bb55a5a7b5

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeVarCounter@
foreign import ccall unsafe "hs_bindgen_4421633e88fc96c4" hs_bindgen_4421633e88fc96c4_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeVarCounter@
hs_bindgen_4421633e88fc96c4 ::
     FC.CInt
  -> IO VarCounter
hs_bindgen_4421633e88fc96c4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4421633e88fc96c4_base

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h 18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter ::
     FC.CInt
     -- ^ __C declaration:__ @start@
  -> IO VarCounter
makeVarCounter = hs_bindgen_4421633e88fc96c4

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_varCounterNext@
foreign import ccall unsafe "hs_bindgen_31edd817cb78027d" hs_bindgen_31edd817cb78027d_base ::
     Ptr.Ptr Void
  -> GHC.Int.Int32
  -> IO GHC.Int.Int32

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_varCounterNext@
hs_bindgen_31edd817cb78027d ::
     VarCounter
  -> FC.CInt
  -> IO FC.CInt
hs_bindgen_31edd817cb78027d =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_31edd817cb78027d_base

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h 19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> FC.CInt
     -- ^ __C declaration:__ @increment@
  -> IO FC.CInt
varCounterNext = hs_bindgen_31edd817cb78027d

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseVarCounter@
foreign import ccall unsafe "hs_bindgen_32e5b257124f69a2" hs_bindgen_32e5b257124f69a2_base ::
     Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseVarCounter@
hs_bindgen_32e5b257124f69a2 ::
     VarCounter
  -> IO ()
hs_bindgen_32e5b257124f69a2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_32e5b257124f69a2_base

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h 20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseVarCounter = hs_bindgen_32e5b257124f69a2
