{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/simple_func.h>"
  , "/* test_functionssimple_func_Example_get_erf */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_97654202e0d79fbb (void)) ("
  , "  double arg1"
  , ")"
  , "{"
  , "  return &erf;"
  , "}"
  , "/* test_functionssimple_func_Example_get_bad_fma */"
  , "__attribute__ ((const))"
  , "double (*hs_bindgen_4c8418e152e9ddbc (void)) ("
  , "  double arg1,"
  , "  double arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &bad_fma;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a271856b1fcc9477 (void)) (void)"
  , "{"
  , "  return &no_args;"
  , "}"
  , "/* test_functionssimple_func_Example_get_no_args_no_void */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_73f2a4bbf4a49702 (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  , "/* test_functionssimple_func_Example_get_fun */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5416cbf886368c01 (void)) ("
  , "  char arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &fun;"
  , "}"
  ]))

-- __unique:__ @test_functionssimple_func_Example_get_erf@
foreign import ccall unsafe "hs_bindgen_97654202e0d79fbb" hs_bindgen_97654202e0d79fbb_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionssimple_func_Example_get_erf@
hs_bindgen_97654202e0d79fbb :: IO (Ptr.FunPtr (FC.CDouble -> IO FC.CDouble))
hs_bindgen_97654202e0d79fbb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_97654202e0d79fbb_base

{-# NOINLINE erf #-}
{-| __C declaration:__ @erf@

    __defined at:__ @functions\/simple_func.h 1:8@

    __exported by:__ @functions\/simple_func.h@
-}
erf :: Ptr.FunPtr (FC.CDouble -> IO FC.CDouble)
erf =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_97654202e0d79fbb

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
foreign import ccall unsafe "hs_bindgen_4c8418e152e9ddbc" hs_bindgen_4c8418e152e9ddbc_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionssimple_func_Example_get_bad_fma@
hs_bindgen_4c8418e152e9ddbc :: IO (Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble))
hs_bindgen_4c8418e152e9ddbc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4c8418e152e9ddbc_base

{-# NOINLINE bad_fma #-}
{-| __C declaration:__ @bad_fma@

    __defined at:__ @functions\/simple_func.h 3:22@

    __exported by:__ @functions\/simple_func.h@
-}
bad_fma :: Ptr.FunPtr (FC.CDouble -> FC.CDouble -> FC.CDouble -> IO FC.CDouble)
bad_fma =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4c8418e152e9ddbc

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
foreign import ccall unsafe "hs_bindgen_a271856b1fcc9477" hs_bindgen_a271856b1fcc9477_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args@
hs_bindgen_a271856b1fcc9477 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_a271856b1fcc9477 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a271856b1fcc9477_base

{-# NOINLINE no_args #-}
{-| __C declaration:__ @no_args@

    __defined at:__ @functions\/simple_func.h 7:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args :: Ptr.FunPtr (IO ())
no_args =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a271856b1fcc9477

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_73f2a4bbf4a49702" hs_bindgen_73f2a4bbf4a49702_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionssimple_func_Example_get_no_args_no_void@
hs_bindgen_73f2a4bbf4a49702 :: IO (Ptr.FunPtr (IO ()))
hs_bindgen_73f2a4bbf4a49702 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_73f2a4bbf4a49702_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @functions\/simple_func.h 9:6@

    __exported by:__ @functions\/simple_func.h@
-}
no_args_no_void :: Ptr.FunPtr (IO ())
no_args_no_void =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_73f2a4bbf4a49702

-- __unique:__ @test_functionssimple_func_Example_get_fun@
foreign import ccall unsafe "hs_bindgen_5416cbf886368c01" hs_bindgen_5416cbf886368c01_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_functionssimple_func_Example_get_fun@
hs_bindgen_5416cbf886368c01 :: IO (Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt))
hs_bindgen_5416cbf886368c01 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5416cbf886368c01_base

{-# NOINLINE fun #-}
{-| __C declaration:__ @fun@

    __defined at:__ @functions\/simple_func.h 11:5@

    __exported by:__ @functions\/simple_func.h@
-}
fun :: Ptr.FunPtr (FC.CChar -> FC.CDouble -> IO FC.CInt)
fun =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5416cbf886368c01
