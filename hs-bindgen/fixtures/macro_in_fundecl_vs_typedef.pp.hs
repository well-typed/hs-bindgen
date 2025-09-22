{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Prelude ((<*>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <macro_in_fundecl_vs_typedef.h>\nchar hs_bindgen_test_macro_in_fundecl_vs_typedef_c7ba346f3006b36f (MC arg1, TC arg2) { return quux1(arg1, arg2); }\nTC hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f (MC arg1, char arg2) { return quux2(arg1, arg2); }\nMC *hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa (float arg1, TC *arg2) { return wam1(arg1, arg2); }\nTC *hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8 (float arg1, MC *arg2) { return wam2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_27a965a9bfd8c176 (struct2 *arg1, MC arg2) { struct_typedef1(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_8e7c55302a7b5716 (struct3_t *arg1, MC arg2) { struct_typedef2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_fe83edb35a817050 (struct4 *arg1, MC arg2) { struct_typedef3(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_162df9fabcbef0c4 (struct struct1 *arg1, MC arg2) { struct_name1(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_a6b5f272333b19d4 (struct struct3 *arg1, MC arg2) { struct_name2(arg1, arg2); }\nvoid hs_bindgen_test_macro_in_fundecl_vs_typedef_9eadc48880259e4a (struct struct4 *arg1, MC arg2) { struct_name3(arg1, arg2); }\n/* get_quux1_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293 (void)) (MC arg1, TC arg2) { return &quux1; } \n/* get_quux2_ptr */ __attribute__ ((const)) TC (*hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b (void)) (MC arg1, char arg2) { return &quux2; } \n/* get_wam1_ptr */ __attribute__ ((const)) MC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe (void)) (float arg1, TC *arg2) { return &wam1; } \n/* get_wam2_ptr */ __attribute__ ((const)) TC *(*hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63 (void)) (float arg1, MC *arg2) { return &wam2; } \n/* get_struct_typedef1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152 (void)) (struct2 *arg1, MC arg2) { return &struct_typedef1; } \n/* get_struct_typedef2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043 (void)) (struct3_t *arg1, MC arg2) { return &struct_typedef2; } \n/* get_struct_typedef3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368 (void)) (struct4 *arg1, MC arg2) { return &struct_typedef3; } \n/* get_struct_name1_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042 (void)) (struct struct1 *arg1, MC arg2) { return &struct_name1; } \n/* get_struct_name2_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2 (void)) (struct struct3 *arg1, MC arg2) { return &struct_name2; } \n/* get_struct_name3_ptr */ __attribute__ ((const)) void (*hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c (void)) (struct struct4 *arg1, MC arg2) { return &struct_name3; } \n")

{-| __C declaration:__ @MC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:4:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype MC = MC
  { un_MC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @TC@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:5:14@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype TC = TC
  { un_TC :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @struct1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:18:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct1 = Struct1
  { struct1_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:18:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1 struct1_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct1_a2

{-| __C declaration:__ @struct2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:19:9@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct2 = Struct2
  { struct2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:19:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2 struct2_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_a2

{-| __C declaration:__ @struct3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct3 = Struct3
  { struct3_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:20:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct3 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct3
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct3 struct3_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct3_a2

{-| __C declaration:__ @struct3_t@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:20:35@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
newtype Struct3_t = Struct3_t
  { un_Struct3_t :: Struct3
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @struct4@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:21:16@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
data Struct4 = Struct4
  { struct4_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @macro_in_fundecl_vs_typedef.h:21:30@

         __exported by:__ @macro_in_fundecl_vs_typedef.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct4 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Struct4
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct4 struct4_a2 ->
            F.pokeByteOff ptr0 (0 :: Int) struct4_a2

{-| __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_c7ba346f3006b36f" quux1
  :: MC
     {- ^ __C declaration:__ @x@
     -}
  -> TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_db114519a8645d1f" quux2
  :: MC
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO TC

{-| __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_0a613fb26d413eaa" wam1
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr MC)

{-| __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_279b15c6940eb4f8" wam2
  :: FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr MC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr TC)

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_27a965a9bfd8c176" struct_typedef1
  :: Ptr.Ptr Struct2
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_8e7c55302a7b5716" struct_typedef2
  :: Ptr.Ptr Struct3_t
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_fe83edb35a817050" struct_typedef3
  :: Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_162df9fabcbef0c4" struct_name1
  :: Ptr.Ptr Struct1
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_a6b5f272333b19d4" struct_name2
  :: Ptr.Ptr Struct3
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macro_in_fundecl_vs_typedef_9eadc48880259e4a" struct_name3
  :: Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293" hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293
  :: IO (Ptr.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

quux1_ptr :: Ptr.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_7d7a63ab896ed293

{-| __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b" hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b
  :: IO (Ptr.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

quux2_ptr :: Ptr.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_b64c564dd7071f5b

{-| __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe" hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

wam1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_aa26b3a0f4d0aefe

{-| __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63" hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63
  :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

wam2_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_5cb5ead73c0a3d63

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152" hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

struct_typedef1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_a1aadeb6878a5152

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043" hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

struct_typedef2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_e1dac8a006e6b043

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368" hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

struct_typedef3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_078075d0a80d4368

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042" hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

struct_name1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_7574edf86480f042

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2" hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

struct_name2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_e7a8c1f45f8b20c2

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c" hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c
  :: IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

struct_name3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_macro_in_fundecl_vs_typedef_d52310663e8daa5c
