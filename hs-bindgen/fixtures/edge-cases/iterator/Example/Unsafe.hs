{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
     RIP.Word8
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeToggle@
hs_bindgen_1b7a6a61a9c0da07 ::
     RIP.CBool
  -> IO Toggle
hs_bindgen_1b7a6a61a9c0da07 =
  RIP.fromFFIType hs_bindgen_1b7a6a61a9c0da07_base

{-| __C declaration:__ @makeToggle@

    __defined at:__ @edge-cases\/iterator.h 4:8@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeToggle ::
     RIP.CBool
     -- ^ __C declaration:__ @start@
  -> IO Toggle
makeToggle = hs_bindgen_1b7a6a61a9c0da07

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_toggleNext@
foreign import ccall unsafe "hs_bindgen_4d2d650f2c8798d6" hs_bindgen_4d2d650f2c8798d6_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Word8

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_toggleNext@
hs_bindgen_4d2d650f2c8798d6 ::
     Toggle
  -> IO RIP.CBool
hs_bindgen_4d2d650f2c8798d6 =
  RIP.fromFFIType hs_bindgen_4d2d650f2c8798d6_base

{-| __C declaration:__ @toggleNext@

    __defined at:__ @edge-cases\/iterator.h 5:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
toggleNext ::
     Toggle
     -- ^ __C declaration:__ @block@
  -> IO RIP.CBool
toggleNext = hs_bindgen_4d2d650f2c8798d6

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseToggle@
foreign import ccall unsafe "hs_bindgen_ddbe11e76502cbdc" hs_bindgen_ddbe11e76502cbdc_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseToggle@
hs_bindgen_ddbe11e76502cbdc ::
     Toggle
  -> IO ()
hs_bindgen_ddbe11e76502cbdc =
  RIP.fromFFIType hs_bindgen_ddbe11e76502cbdc_base

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
     RIP.Int32
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeCounter@
hs_bindgen_2b04d558934551d2 ::
     RIP.CInt
  -> RIP.CInt
  -> IO Counter
hs_bindgen_2b04d558934551d2 =
  RIP.fromFFIType hs_bindgen_2b04d558934551d2_base

{-| __C declaration:__ @makeCounter@

    __defined at:__ @edge-cases\/iterator.h 11:9@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeCounter ::
     RIP.CInt
     -- ^ __C declaration:__ @start@
  -> RIP.CInt
     -- ^ __C declaration:__ @increment@
  -> IO Counter
makeCounter = hs_bindgen_2b04d558934551d2

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_counterNext@
foreign import ccall unsafe "hs_bindgen_5bba69c8bfbeedf0" hs_bindgen_5bba69c8bfbeedf0_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_counterNext@
hs_bindgen_5bba69c8bfbeedf0 ::
     Counter
  -> IO RIP.CInt
hs_bindgen_5bba69c8bfbeedf0 =
  RIP.fromFFIType hs_bindgen_5bba69c8bfbeedf0_base

{-| __C declaration:__ @counterNext@

    __defined at:__ @edge-cases\/iterator.h 12:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
counterNext ::
     Counter
     -- ^ __C declaration:__ @block@
  -> IO RIP.CInt
counterNext = hs_bindgen_5bba69c8bfbeedf0

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseCounter@
foreign import ccall unsafe "hs_bindgen_429845bb55a5a7b5" hs_bindgen_429845bb55a5a7b5_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseCounter@
hs_bindgen_429845bb55a5a7b5 ::
     Counter
  -> IO ()
hs_bindgen_429845bb55a5a7b5 =
  RIP.fromFFIType hs_bindgen_429845bb55a5a7b5_base

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
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_makeVarCounter@
hs_bindgen_4421633e88fc96c4 ::
     RIP.CInt
  -> IO VarCounter
hs_bindgen_4421633e88fc96c4 =
  RIP.fromFFIType hs_bindgen_4421633e88fc96c4_base

{-| __C declaration:__ @makeVarCounter@

    __defined at:__ @edge-cases\/iterator.h 18:12@

    __exported by:__ @edge-cases\/iterator.h@
-}
makeVarCounter ::
     RIP.CInt
     -- ^ __C declaration:__ @start@
  -> IO VarCounter
makeVarCounter = hs_bindgen_4421633e88fc96c4

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_varCounterNext@
foreign import ccall unsafe "hs_bindgen_31edd817cb78027d" hs_bindgen_31edd817cb78027d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> IO RIP.Int32

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_varCounterNext@
hs_bindgen_31edd817cb78027d ::
     VarCounter
  -> RIP.CInt
  -> IO RIP.CInt
hs_bindgen_31edd817cb78027d =
  RIP.fromFFIType hs_bindgen_31edd817cb78027d_base

{-| __C declaration:__ @varCounterNext@

    __defined at:__ @edge-cases\/iterator.h 19:5@

    __exported by:__ @edge-cases\/iterator.h@
-}
varCounterNext ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> RIP.CInt
     -- ^ __C declaration:__ @increment@
  -> IO RIP.CInt
varCounterNext = hs_bindgen_31edd817cb78027d

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseVarCounter@
foreign import ccall unsafe "hs_bindgen_32e5b257124f69a2" hs_bindgen_32e5b257124f69a2_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_edgecasesiterator_Example_Unsafe_releaseVarCounter@
hs_bindgen_32e5b257124f69a2 ::
     VarCounter
  -> IO ()
hs_bindgen_32e5b257124f69a2 =
  RIP.fromFFIType hs_bindgen_32e5b257124f69a2_base

{-| __C declaration:__ @releaseVarCounter@

    __defined at:__ @edge-cases\/iterator.h 20:6@

    __exported by:__ @edge-cases\/iterator.h@
-}
releaseVarCounter ::
     VarCounter
     -- ^ __C declaration:__ @block@
  -> IO ()
releaseVarCounter = hs_bindgen_32e5b257124f69a2
