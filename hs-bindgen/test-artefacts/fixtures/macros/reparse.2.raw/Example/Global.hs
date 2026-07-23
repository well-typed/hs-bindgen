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

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/reparse.h>"
  , "/* test_macrosreparse_2_raw_Example_get_global1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_93cfa28c4d516159 (void)"
  , "{"
  , "  return &global1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_global2 */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_9d8438d8f0c6d639 (void)"
  , "{"
  , "  return &global2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_global3 */"
  , "__attribute__ ((const))"
  , "signed int ***hs_bindgen_29516306ddb7335f (void)"
  , "{"
  , "  return &global3;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const1 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_3326ba6dde505604 (void)"
  , "{"
  , "  return &const1;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const2 */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_8add8e4cd4511ebc (void)"
  , "{"
  , "  return &const2;"
  , "}"
  , "/* test_macrosreparse_2_raw_Example_get_const3 */"
  , "__attribute__ ((const))"
  , "signed int **const *hs_bindgen_e00a055156bc89e4 (void)"
  , "{"
  , "  return &const3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global1@
foreign import ccall unsafe "hs_bindgen_93cfa28c4d516159" hs_bindgen_93cfa28c4d516159_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global1@
hs_bindgen_93cfa28c4d516159 :: IO (BG.Ptr BG.CInt)
hs_bindgen_93cfa28c4d516159 =
  BG.fromFFIType hs_bindgen_93cfa28c4d516159_base

{-# NOINLINE global1 #-}
{-| Globals and constants

    __C declaration:__ @global1@

    __defined at:__ @macros\/reparse.h 161:13@

    __exported by:__ @macros\/reparse.h@
-}
global1 :: BG.Ptr BG.CInt
global1 =
  BG.unsafePerformIO hs_bindgen_93cfa28c4d516159

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global2@
foreign import ccall unsafe "hs_bindgen_9d8438d8f0c6d639" hs_bindgen_9d8438d8f0c6d639_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global2@
hs_bindgen_9d8438d8f0c6d639 :: IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_9d8438d8f0c6d639 =
  BG.fromFFIType hs_bindgen_9d8438d8f0c6d639_base

{-# NOINLINE global2 #-}
{-| __C declaration:__ @global2@

    __defined at:__ @macros\/reparse.h 162:13@

    __exported by:__ @macros\/reparse.h@
-}
global2 :: BG.Ptr (BG.Ptr BG.CInt)
global2 =
  BG.unsafePerformIO hs_bindgen_9d8438d8f0c6d639

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global3@
foreign import ccall unsafe "hs_bindgen_29516306ddb7335f" hs_bindgen_29516306ddb7335f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_global3@
hs_bindgen_29516306ddb7335f :: IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_29516306ddb7335f =
  BG.fromFFIType hs_bindgen_29516306ddb7335f_base

{-# NOINLINE global3 #-}
{-| __C declaration:__ @global3@

    __defined at:__ @macros\/reparse.h 163:13@

    __exported by:__ @macros\/reparse.h@
-}
global3 :: BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))
global3 =
  BG.unsafePerformIO hs_bindgen_29516306ddb7335f

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const1@
foreign import ccall unsafe "hs_bindgen_3326ba6dde505604" hs_bindgen_3326ba6dde505604_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const1@
hs_bindgen_3326ba6dde505604 :: IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_3326ba6dde505604 =
  BG.fromFFIType hs_bindgen_3326ba6dde505604_base

{-# NOINLINE hs_bindgen_0ad29e3e91a18217 #-}
{-| __C declaration:__ @const1@

    __defined at:__ @macros\/reparse.h 165:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_2_raw_Example_const1@
-}
hs_bindgen_0ad29e3e91a18217 :: PtrConst.PtrConst BG.CInt
hs_bindgen_0ad29e3e91a18217 =
  BG.unsafePerformIO hs_bindgen_3326ba6dde505604

{-# NOINLINE const1 #-}
const1 :: BG.CInt
const1 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_0ad29e3e91a18217)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const2@
foreign import ccall unsafe "hs_bindgen_8add8e4cd4511ebc" hs_bindgen_8add8e4cd4511ebc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const2@
hs_bindgen_8add8e4cd4511ebc :: IO (PtrConst.PtrConst (BG.Ptr BG.CInt))
hs_bindgen_8add8e4cd4511ebc =
  BG.fromFFIType hs_bindgen_8add8e4cd4511ebc_base

{-# NOINLINE hs_bindgen_67e8005e9795d40a #-}
{-| __C declaration:__ @const2@

    __defined at:__ @macros\/reparse.h 166:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_2_raw_Example_const2@
-}
hs_bindgen_67e8005e9795d40a :: PtrConst.PtrConst (BG.Ptr BG.CInt)
hs_bindgen_67e8005e9795d40a =
  BG.unsafePerformIO hs_bindgen_8add8e4cd4511ebc

{-# NOINLINE const2 #-}
const2 :: BG.Ptr BG.CInt
const2 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_67e8005e9795d40a)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const3@
foreign import ccall unsafe "hs_bindgen_e00a055156bc89e4" hs_bindgen_e00a055156bc89e4_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_2_raw_Example_get_const3@
hs_bindgen_e00a055156bc89e4 :: IO (PtrConst.PtrConst (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_e00a055156bc89e4 =
  BG.fromFFIType hs_bindgen_e00a055156bc89e4_base

{-# NOINLINE hs_bindgen_2f3c7514e0b7b99c #-}
{-| __C declaration:__ @const3@

    __defined at:__ @macros\/reparse.h 167:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_2_raw_Example_const3@
-}
hs_bindgen_2f3c7514e0b7b99c :: PtrConst.PtrConst (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_2f3c7514e0b7b99c =
  BG.unsafePerformIO hs_bindgen_e00a055156bc89e4

{-# NOINLINE const3 #-}
const3 :: BG.Ptr (BG.Ptr BG.CInt)
const3 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_2f3c7514e0b7b99c)
