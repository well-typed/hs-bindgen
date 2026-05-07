{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.global1
    , Example.Global.global2
    , Example.Global.global3
    , Example.Global.const1
    , Example.Global.const2
    , Example.Global.const3
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "/* test_macrosreparse_Example_get_global1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_feda7c77651645fe (void)"
  , "{"
  , "  return &global1;"
  , "}"
  , "/* test_macrosreparse_Example_get_global2 */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_61b4abc5e1afa043 (void)"
  , "{"
  , "  return &global2;"
  , "}"
  , "/* test_macrosreparse_Example_get_global3 */"
  , "__attribute__ ((const))"
  , "signed int ***hs_bindgen_f9bcb9580741dbc1 (void)"
  , "{"
  , "  return &global3;"
  , "}"
  , "/* test_macrosreparse_Example_get_const1 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_044004234e2019f6 (void)"
  , "{"
  , "  return &const1;"
  , "}"
  , "/* test_macrosreparse_Example_get_const2 */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_6c69b6e35fe122c5 (void)"
  , "{"
  , "  return &const2;"
  , "}"
  , "/* test_macrosreparse_Example_get_const3 */"
  , "__attribute__ ((const))"
  , "signed int **const *hs_bindgen_da21ad94b09d46ae (void)"
  , "{"
  , "  return &const3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_Example_get_global1@
foreign import ccall unsafe "hs_bindgen_feda7c77651645fe" hs_bindgen_feda7c77651645fe_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_global1@
hs_bindgen_feda7c77651645fe :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_feda7c77651645fe =
  RIP.fromFFIType hs_bindgen_feda7c77651645fe_base

{-# NOINLINE global1 #-}
{-| Globals and constants

    __C declaration:__ @global1@

    __defined at:__ @macros\/reparse.h 161:13@

    __exported by:__ @macros\/reparse.h@
-}
global1 :: RIP.Ptr RIP.CInt
global1 =
  RIP.unsafePerformIO hs_bindgen_feda7c77651645fe

-- __unique:__ @test_macrosreparse_Example_get_global2@
foreign import ccall unsafe "hs_bindgen_61b4abc5e1afa043" hs_bindgen_61b4abc5e1afa043_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_global2@
hs_bindgen_61b4abc5e1afa043 :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_61b4abc5e1afa043 =
  RIP.fromFFIType hs_bindgen_61b4abc5e1afa043_base

{-# NOINLINE global2 #-}
{-| __C declaration:__ @global2@

    __defined at:__ @macros\/reparse.h 162:13@

    __exported by:__ @macros\/reparse.h@
-}
global2 :: RIP.Ptr (RIP.Ptr RIP.CInt)
global2 =
  RIP.unsafePerformIO hs_bindgen_61b4abc5e1afa043

-- __unique:__ @test_macrosreparse_Example_get_global3@
foreign import ccall unsafe "hs_bindgen_f9bcb9580741dbc1" hs_bindgen_f9bcb9580741dbc1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_global3@
hs_bindgen_f9bcb9580741dbc1 :: IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_f9bcb9580741dbc1 =
  RIP.fromFFIType hs_bindgen_f9bcb9580741dbc1_base

{-# NOINLINE global3 #-}
{-| __C declaration:__ @global3@

    __defined at:__ @macros\/reparse.h 163:13@

    __exported by:__ @macros\/reparse.h@
-}
global3 :: RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))
global3 =
  RIP.unsafePerformIO hs_bindgen_f9bcb9580741dbc1

-- __unique:__ @test_macrosreparse_Example_get_const1@
foreign import ccall unsafe "hs_bindgen_044004234e2019f6" hs_bindgen_044004234e2019f6_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const1@
hs_bindgen_044004234e2019f6 :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_044004234e2019f6 =
  RIP.fromFFIType hs_bindgen_044004234e2019f6_base

{-# NOINLINE hs_bindgen_f883e611325aa029 #-}
{-| __C declaration:__ @const1@

    __defined at:__ @macros\/reparse.h 165:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_Example_const1@
-}
hs_bindgen_f883e611325aa029 :: PtrConst.PtrConst RIP.CInt
hs_bindgen_f883e611325aa029 =
  RIP.unsafePerformIO hs_bindgen_044004234e2019f6

{-# NOINLINE const1 #-}
const1 :: RIP.CInt
const1 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_f883e611325aa029)

-- __unique:__ @test_macrosreparse_Example_get_const2@
foreign import ccall unsafe "hs_bindgen_6c69b6e35fe122c5" hs_bindgen_6c69b6e35fe122c5_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const2@
hs_bindgen_6c69b6e35fe122c5 :: IO (PtrConst.PtrConst (RIP.Ptr RIP.CInt))
hs_bindgen_6c69b6e35fe122c5 =
  RIP.fromFFIType hs_bindgen_6c69b6e35fe122c5_base

{-# NOINLINE hs_bindgen_4a0e98a60a6414ee #-}
{-| __C declaration:__ @const2@

    __defined at:__ @macros\/reparse.h 166:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_Example_const2@
-}
hs_bindgen_4a0e98a60a6414ee :: PtrConst.PtrConst (RIP.Ptr RIP.CInt)
hs_bindgen_4a0e98a60a6414ee =
  RIP.unsafePerformIO hs_bindgen_6c69b6e35fe122c5

{-# NOINLINE const2 #-}
const2 :: RIP.Ptr RIP.CInt
const2 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_4a0e98a60a6414ee)

-- __unique:__ @test_macrosreparse_Example_get_const3@
foreign import ccall unsafe "hs_bindgen_da21ad94b09d46ae" hs_bindgen_da21ad94b09d46ae_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosreparse_Example_get_const3@
hs_bindgen_da21ad94b09d46ae :: IO (PtrConst.PtrConst (RIP.Ptr (RIP.Ptr RIP.CInt)))
hs_bindgen_da21ad94b09d46ae =
  RIP.fromFFIType hs_bindgen_da21ad94b09d46ae_base

{-# NOINLINE hs_bindgen_0d6ba48ba79ada14 #-}
{-| __C declaration:__ @const3@

    __defined at:__ @macros\/reparse.h 167:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_Example_const3@
-}
hs_bindgen_0d6ba48ba79ada14 :: PtrConst.PtrConst (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_0d6ba48ba79ada14 =
  RIP.unsafePerformIO hs_bindgen_da21ad94b09d46ae

{-# NOINLINE const3 #-}
const3 :: RIP.Ptr (RIP.Ptr RIP.CInt)
const3 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_0d6ba48ba79ada14)
