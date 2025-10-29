{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.Prelude
import Prelude (unlines)
import Prelude ((<*>), Eq, IO, Int, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource (Prelude.unlines
  [ "#include <struct_arg.h>"
  , "signed int hs_bindgen_test_struct_arg_61dfa2c4506feb8f ("
  , "  struct thing *arg1"
  , ")"
  , "{"
  , "  return thing_fun_1(*arg1);"
  , "}"
  , "void hs_bindgen_test_struct_arg_022cc8107f565c95 ("
  , "  signed int arg1,"
  , "  struct thing *arg2"
  , ")"
  , "{"
  , "  *arg2 = thing_fun_2(arg1);"
  , "}"
  , "void hs_bindgen_test_struct_arg_4d9304280cca3098 ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3,"
  , "  struct thing *arg4"
  , ")"
  , "{"
  , "  *arg4 = thing_fun_3a(arg1, *arg2, arg3);"
  , "}"
  , "char hs_bindgen_test_struct_arg_f39687b254852452 ("
  , "  signed int arg1,"
  , "  struct thing *arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return thing_fun_3b(arg1, *arg2, arg3);"
  , "}"
  , "/* get_thing_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_struct_arg_c5f0c295b311010a (void)) ("
  , "  struct thing arg1"
  , ")"
  , "{"
  , "  return &thing_fun_1;"
  , "}"
  , "/* get_thing_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_test_struct_arg_24edf6600396b62a (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &thing_fun_2;"
  , "}"
  , "/* get_thing_fun_3a_ptr */"
  , "__attribute__ ((const))"
  , "struct thing (*hs_bindgen_test_struct_arg_29a42b48992cd0bf (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3a;"
  , "}"
  , "/* get_thing_fun_3b_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_struct_arg_0d6597dfc03e312f (void)) ("
  , "  signed int arg1,"
  , "  struct thing arg2,"
  , "  double arg3"
  , ")"
  , "{"
  , "  return &thing_fun_3b;"
  , "}"
  ]))

{-| __C declaration:__ @thing@

    __defined at:__ @struct_arg.h:2:8@

    __exported by:__ @struct_arg.h@
-}
data Thing = Thing
  { thing_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @struct_arg.h:3:9@

         __exported by:__ @struct_arg.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Thing where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Thing
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Thing thing_x2 ->
            F.pokeByteOff ptr0 (0 :: Int) thing_x2

{-| Pointer-based API for 'thing_fun_1'

-}
foreign import ccall safe "hs_bindgen_test_struct_arg_61dfa2c4506feb8f" thing_fun_1_wrapper ::
     Ptr.Ptr Thing
  -> IO FC.CInt

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @struct_arg.h:6:5@

    __exported by:__ @struct_arg.h@
-}
thing_fun_1 ::
     Thing
     {- ^ __C declaration:__ @x@
     -}
  -> IO FC.CInt
thing_fun_1 =
  \x0 -> F.with x0 (\y1 -> thing_fun_1_wrapper y1)

{-| Pointer-based API for 'thing_fun_2'

-}
foreign import ccall safe "hs_bindgen_test_struct_arg_022cc8107f565c95" thing_fun_2_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> IO ()

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @struct_arg.h:7:14@

    __exported by:__ @struct_arg.h@
-}
thing_fun_2 ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> IO Thing
thing_fun_2 =
  \x0 ->
    HsBindgen.Runtime.CAPI.allocaAndPeek (\z1 ->
                                            thing_fun_2_wrapper x0 z1)

{-| Pointer-based API for 'thing_fun_3a'

-}
foreign import ccall safe "hs_bindgen_test_struct_arg_4d9304280cca3098" thing_fun_3a_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> Ptr.Ptr Thing
  -> IO ()

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @struct_arg.h:9:14@

    __exported by:__ @struct_arg.h@
-}
thing_fun_3a ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO Thing
thing_fun_3a =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 ->
                     HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                             thing_fun_3a_wrapper x0 y3 x2 z4))

{-| Pointer-based API for 'thing_fun_3b'

-}
foreign import ccall safe "hs_bindgen_test_struct_arg_f39687b254852452" thing_fun_3b_wrapper ::
     FC.CInt
  -> Ptr.Ptr Thing
  -> FC.CDouble
  -> IO FC.CChar

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @struct_arg.h:10:6@

    __exported by:__ @struct_arg.h@
-}
thing_fun_3b ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Thing
     {- ^ __C declaration:__ @y@
     -}
  -> FC.CDouble
     {- ^ __C declaration:__ @z@
     -}
  -> IO FC.CChar
thing_fun_3b =
  \x0 ->
    \x1 ->
      \x2 ->
        F.with x1 (\y3 -> thing_fun_3b_wrapper x0 y3 x2)

foreign import ccall unsafe "hs_bindgen_test_struct_arg_c5f0c295b311010a" hs_bindgen_test_struct_arg_c5f0c295b311010a ::
     IO (Ptr.FunPtr (Thing -> IO FC.CInt))

{-# NOINLINE thing_fun_1_ptr #-}

{-| __C declaration:__ @thing_fun_1@

    __defined at:__ @struct_arg.h:6:5@

    __exported by:__ @struct_arg.h@
-}
thing_fun_1_ptr :: Ptr.FunPtr (Thing -> IO FC.CInt)
thing_fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_c5f0c295b311010a

foreign import ccall unsafe "hs_bindgen_test_struct_arg_24edf6600396b62a" hs_bindgen_test_struct_arg_24edf6600396b62a ::
     IO (Ptr.FunPtr (FC.CInt -> IO Thing))

{-# NOINLINE thing_fun_2_ptr #-}

{-| __C declaration:__ @thing_fun_2@

    __defined at:__ @struct_arg.h:7:14@

    __exported by:__ @struct_arg.h@
-}
thing_fun_2_ptr :: Ptr.FunPtr (FC.CInt -> IO Thing)
thing_fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_24edf6600396b62a

foreign import ccall unsafe "hs_bindgen_test_struct_arg_29a42b48992cd0bf" hs_bindgen_test_struct_arg_29a42b48992cd0bf ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing))

{-# NOINLINE thing_fun_3a_ptr #-}

{-| __C declaration:__ @thing_fun_3a@

    __defined at:__ @struct_arg.h:9:14@

    __exported by:__ @struct_arg.h@
-}
thing_fun_3a_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO Thing)
thing_fun_3a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_29a42b48992cd0bf

foreign import ccall unsafe "hs_bindgen_test_struct_arg_0d6597dfc03e312f" hs_bindgen_test_struct_arg_0d6597dfc03e312f ::
     IO (Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar))

{-# NOINLINE thing_fun_3b_ptr #-}

{-| __C declaration:__ @thing_fun_3b@

    __defined at:__ @struct_arg.h:10:6@

    __exported by:__ @struct_arg.h@
-}
thing_fun_3b_ptr :: Ptr.FunPtr (FC.CInt -> Thing -> FC.CDouble -> IO FC.CChar)
thing_fun_3b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_struct_arg_0d6597dfc03e312f
