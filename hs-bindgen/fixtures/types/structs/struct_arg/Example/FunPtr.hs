{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "/* Example_get_thing_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_typesstructsstruct_arg_b246bdb8a7da143b (void)) ("
  , "  struct thing arg1"
  , ")"
  , "{"
  , "  return &thing_fun_1;"
  , "}"
  , "/* Example_get_thing_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_test_typesstructsstruct_arg_dbea4646c9e4aa38 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &thing_fun_2;"
  , "}"
  , "/* Example_get_thing_fun_3a_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_test_typesstructsstruct_arg_c108b151da528327 (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3a;"
  , "}"
  , "/* Example_get_thing_fun_3b_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_typesstructsstruct_arg_bd55e92713dbbc36 (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3b;"
  , "}"
  ]))

{-| __unique:__ @Example_get_thing_fun_1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesstructsstruct_arg_b246bdb8a7da143b" hs_bindgen_test_typesstructsstruct_arg_b246bdb8a7da143b ::
     IO (Ptr.FunPtr (Thing -> IO FC.CInt))

{-# NOINLINE thing_fun_1_ptr #-}

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h:6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1_ptr :: Ptr.FunPtr (Thing -> IO FC.CInt)
thing_fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesstructsstruct_arg_b246bdb8a7da143b

{-| __unique:__ @Example_get_thing_fun_2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesstructsstruct_arg_dbea4646c9e4aa38" hs_bindgen_test_typesstructsstruct_arg_dbea4646c9e4aa38 ::
     IO (Ptr.FunPtr (FC.CInt -> IO Thing))

{-# NOINLINE thing_fun_2_ptr #-}

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h:7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2_ptr :: Ptr.FunPtr (FC.CInt -> IO Thing)
thing_fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesstructsstruct_arg_dbea4646c9e4aa38

{-| __unique:__ @Example_get_thing_fun_3a_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesstructsstruct_arg_c108b151da528327" hs_bindgen_test_typesstructsstruct_arg_c108b151da528327 ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing))

{-# NOINLINE thing_fun_3a_ptr #-}

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h:9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing)
thing_fun_3a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesstructsstruct_arg_c108b151da528327

{-| __unique:__ @Example_get_thing_fun_3b_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typesstructsstruct_arg_bd55e92713dbbc36" hs_bindgen_test_typesstructsstruct_arg_bd55e92713dbbc36 ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar))

{-# NOINLINE thing_fun_3b_ptr #-}

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h:10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar)
thing_fun_3b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typesstructsstruct_arg_bd55e92713dbbc36
