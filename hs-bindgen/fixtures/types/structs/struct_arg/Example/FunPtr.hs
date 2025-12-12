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
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_025103be0a357629 (void)) ("
  , "  struct thing arg1"
  , ")"
  , "{"
  , "  return &thing_fun_1;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_e66f3bfa6ad4e4c8 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &thing_fun_2;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_3a_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_5fea3576dcdc292f (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3a;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_3b_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_8df67f0e3a4b504f (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3b;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_025103be0a357629" hs_bindgen_025103be0a357629_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_1_ptr@
hs_bindgen_025103be0a357629 ::
     IO (Ptr.FunPtr (Thing -> IO FC.CInt))
hs_bindgen_025103be0a357629 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_025103be0a357629_base

{-# NOINLINE thing_fun_1_ptr #-}

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h:6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1_ptr :: Ptr.FunPtr (Thing -> IO FC.CInt)
thing_fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_025103be0a357629

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e66f3bfa6ad4e4c8" hs_bindgen_e66f3bfa6ad4e4c8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_2_ptr@
hs_bindgen_e66f3bfa6ad4e4c8 ::
     IO (Ptr.FunPtr (FC.CInt -> IO Thing))
hs_bindgen_e66f3bfa6ad4e4c8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e66f3bfa6ad4e4c8_base

{-# NOINLINE thing_fun_2_ptr #-}

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h:7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2_ptr :: Ptr.FunPtr (FC.CInt -> IO Thing)
thing_fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e66f3bfa6ad4e4c8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5fea3576dcdc292f" hs_bindgen_5fea3576dcdc292f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3a_ptr@
hs_bindgen_5fea3576dcdc292f ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing))
hs_bindgen_5fea3576dcdc292f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_5fea3576dcdc292f_base

{-# NOINLINE thing_fun_3a_ptr #-}

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h:9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing)
thing_fun_3a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5fea3576dcdc292f

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8df67f0e3a4b504f" hs_bindgen_8df67f0e3a4b504f_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3b_ptr@
hs_bindgen_8df67f0e3a4b504f ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar))
hs_bindgen_8df67f0e3a4b504f =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8df67f0e3a4b504f_base

{-# NOINLINE thing_fun_3b_ptr #-}

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h:10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar)
thing_fun_3b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8df67f0e3a4b504f
