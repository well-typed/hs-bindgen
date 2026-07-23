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
  , "/* test_macrosreparse_1_empty_Example_get_global1 */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_c2929de3db3ebc68 (void)"
  , "{"
  , "  return &global1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_global2 */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_b642618eb14a88bf (void)"
  , "{"
  , "  return &global2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_global3 */"
  , "__attribute__ ((const))"
  , "signed int ***hs_bindgen_37e22dedff20b0fa (void)"
  , "{"
  , "  return &global3;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const1 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_f54afb44a4459d9c (void)"
  , "{"
  , "  return &const1;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const2 */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_139264d86115f755 (void)"
  , "{"
  , "  return &const2;"
  , "}"
  , "/* test_macrosreparse_1_empty_Example_get_const3 */"
  , "__attribute__ ((const))"
  , "signed int **const *hs_bindgen_a13825d736275018 (void)"
  , "{"
  , "  return &const3;"
  , "}"
  ]))

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global1@
foreign import ccall unsafe "hs_bindgen_c2929de3db3ebc68" hs_bindgen_c2929de3db3ebc68_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global1@
hs_bindgen_c2929de3db3ebc68 :: IO (BG.Ptr BG.CInt)
hs_bindgen_c2929de3db3ebc68 =
  BG.fromFFIType hs_bindgen_c2929de3db3ebc68_base

{-# NOINLINE global1 #-}
{-| Globals and constants

    __C declaration:__ @global1@

    __defined at:__ @macros\/reparse.h 161:13@

    __exported by:__ @macros\/reparse.h@
-}
global1 :: BG.Ptr BG.CInt
global1 =
  BG.unsafePerformIO hs_bindgen_c2929de3db3ebc68

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global2@
foreign import ccall unsafe "hs_bindgen_b642618eb14a88bf" hs_bindgen_b642618eb14a88bf_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global2@
hs_bindgen_b642618eb14a88bf :: IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_b642618eb14a88bf =
  BG.fromFFIType hs_bindgen_b642618eb14a88bf_base

{-# NOINLINE global2 #-}
{-| __C declaration:__ @global2@

    __defined at:__ @macros\/reparse.h 162:13@

    __exported by:__ @macros\/reparse.h@
-}
global2 :: BG.Ptr (BG.Ptr BG.CInt)
global2 =
  BG.unsafePerformIO hs_bindgen_b642618eb14a88bf

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global3@
foreign import ccall unsafe "hs_bindgen_37e22dedff20b0fa" hs_bindgen_37e22dedff20b0fa_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_global3@
hs_bindgen_37e22dedff20b0fa :: IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_37e22dedff20b0fa =
  BG.fromFFIType hs_bindgen_37e22dedff20b0fa_base

{-# NOINLINE global3 #-}
{-| __C declaration:__ @global3@

    __defined at:__ @macros\/reparse.h 163:13@

    __exported by:__ @macros\/reparse.h@
-}
global3 :: BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))
global3 =
  BG.unsafePerformIO hs_bindgen_37e22dedff20b0fa

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const1@
foreign import ccall unsafe "hs_bindgen_f54afb44a4459d9c" hs_bindgen_f54afb44a4459d9c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const1@
hs_bindgen_f54afb44a4459d9c :: IO (PtrConst.PtrConst BG.CInt)
hs_bindgen_f54afb44a4459d9c =
  BG.fromFFIType hs_bindgen_f54afb44a4459d9c_base

{-# NOINLINE hs_bindgen_c9e1c2f1b5f1a18b #-}
{-| __C declaration:__ @const1@

    __defined at:__ @macros\/reparse.h 165:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_1_empty_Example_const1@
-}
hs_bindgen_c9e1c2f1b5f1a18b :: PtrConst.PtrConst BG.CInt
hs_bindgen_c9e1c2f1b5f1a18b =
  BG.unsafePerformIO hs_bindgen_f54afb44a4459d9c

{-# NOINLINE const1 #-}
const1 :: BG.CInt
const1 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_c9e1c2f1b5f1a18b)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const2@
foreign import ccall unsafe "hs_bindgen_139264d86115f755" hs_bindgen_139264d86115f755_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const2@
hs_bindgen_139264d86115f755 :: IO (PtrConst.PtrConst (BG.Ptr BG.CInt))
hs_bindgen_139264d86115f755 =
  BG.fromFFIType hs_bindgen_139264d86115f755_base

{-# NOINLINE hs_bindgen_e27452ffe7a65d32 #-}
{-| __C declaration:__ @const2@

    __defined at:__ @macros\/reparse.h 166:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_1_empty_Example_const2@
-}
hs_bindgen_e27452ffe7a65d32 :: PtrConst.PtrConst (BG.Ptr BG.CInt)
hs_bindgen_e27452ffe7a65d32 =
  BG.unsafePerformIO hs_bindgen_139264d86115f755

{-# NOINLINE const2 #-}
const2 :: BG.Ptr BG.CInt
const2 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_e27452ffe7a65d32)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const3@
foreign import ccall unsafe "hs_bindgen_a13825d736275018" hs_bindgen_a13825d736275018_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosreparse_1_empty_Example_get_const3@
hs_bindgen_a13825d736275018 :: IO (PtrConst.PtrConst (BG.Ptr (BG.Ptr BG.CInt)))
hs_bindgen_a13825d736275018 =
  BG.fromFFIType hs_bindgen_a13825d736275018_base

{-# NOINLINE hs_bindgen_217857747c06d0ce #-}
{-| __C declaration:__ @const3@

    __defined at:__ @macros\/reparse.h 167:19@

    __exported by:__ @macros\/reparse.h@

    __unique:__ @test_macrosreparse_1_empty_Example_const3@
-}
hs_bindgen_217857747c06d0ce :: PtrConst.PtrConst (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_217857747c06d0ce =
  BG.unsafePerformIO hs_bindgen_a13825d736275018

{-# NOINLINE const3 #-}
const3 :: BG.Ptr (BG.Ptr BG.CInt)
const3 =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_217857747c06d0ce)
