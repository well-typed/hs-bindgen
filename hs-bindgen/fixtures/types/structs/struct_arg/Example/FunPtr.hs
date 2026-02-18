{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/structs/struct_arg.h>"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_17a14e7ffaa1e2e5 (void)) ("
  , "  struct thing arg1"
  , ")"
  , "{"
  , "  return &thing_fun_1;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_2 */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_91748ffd57eae163 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &thing_fun_2;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_3a */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_84d98a03314ff518 (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3a;"
  , "}"
  , "/* test_typesstructsstruct_arg_Example_get_thing_fun_3b */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_855ae9088b15005e (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3b;"
  , "}"
  ]))

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_1@
foreign import ccall unsafe "hs_bindgen_17a14e7ffaa1e2e5" hs_bindgen_17a14e7ffaa1e2e5_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_1@
hs_bindgen_17a14e7ffaa1e2e5 :: IO (RIP.FunPtr (Thing -> IO RIP.CInt))
hs_bindgen_17a14e7ffaa1e2e5 =
  RIP.fromFFIType hs_bindgen_17a14e7ffaa1e2e5_base

{-# NOINLINE thing_fun_1 #-}
{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @types\/structs\/struct_arg.h 6:5@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_1 :: RIP.FunPtr (Thing -> IO RIP.CInt)
thing_fun_1 =
  RIP.unsafePerformIO hs_bindgen_17a14e7ffaa1e2e5

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_2@
foreign import ccall unsafe "hs_bindgen_91748ffd57eae163" hs_bindgen_91748ffd57eae163_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_2@
hs_bindgen_91748ffd57eae163 :: IO (RIP.FunPtr (RIP.CInt -> IO Thing))
hs_bindgen_91748ffd57eae163 =
  RIP.fromFFIType hs_bindgen_91748ffd57eae163_base

{-# NOINLINE thing_fun_2 #-}
{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @types\/structs\/struct_arg.h 7:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_2 :: RIP.FunPtr (RIP.CInt -> IO Thing)
thing_fun_2 =
  RIP.unsafePerformIO hs_bindgen_91748ffd57eae163

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3a@
foreign import ccall unsafe "hs_bindgen_84d98a03314ff518" hs_bindgen_84d98a03314ff518_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3a@
hs_bindgen_84d98a03314ff518 :: IO (RIP.FunPtr (RIP.CInt -> Thing -> RIP.CDouble -> IO Thing))
hs_bindgen_84d98a03314ff518 =
  RIP.fromFFIType hs_bindgen_84d98a03314ff518_base

{-# NOINLINE thing_fun_3a #-}
{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @types\/structs\/struct_arg.h 9:14@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3a :: RIP.FunPtr (RIP.CInt -> Thing -> RIP.CDouble -> IO Thing)
thing_fun_3a =
  RIP.unsafePerformIO hs_bindgen_84d98a03314ff518

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3b@
foreign import ccall unsafe "hs_bindgen_855ae9088b15005e" hs_bindgen_855ae9088b15005e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typesstructsstruct_arg_Example_get_thing_fun_3b@
hs_bindgen_855ae9088b15005e :: IO (RIP.FunPtr (RIP.CInt -> Thing -> RIP.CDouble -> IO RIP.CChar))
hs_bindgen_855ae9088b15005e =
  RIP.fromFFIType hs_bindgen_855ae9088b15005e_base

{-# NOINLINE thing_fun_3b #-}
{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @types\/structs\/struct_arg.h 10:6@

    __exported by:__ @types\/structs\/struct_arg.h@
-}
thing_fun_3b :: RIP.FunPtr (RIP.CInt -> Thing -> RIP.CDouble -> IO RIP.CChar)
thing_fun_3b =
  RIP.unsafePerformIO hs_bindgen_855ae9088b15005e
