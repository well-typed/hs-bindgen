{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import Prelude (unlines)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

$(HsBindgen.Runtime.Prelude.addCSource (Prelude.unlines
  [ "#include <distilled_lib_1.h>"
  , "int32_t hs_bindgen_test_distilled_lib_1_29c178c31334688f ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t *arg3"
  , ")"
  , "{"
  , "  return some_fun(arg1, arg2, arg3);"
  , "}"
  , "/* get_some_fun_ptr */"
  , "__attribute__ ((const))"
  , "int32_t (*hs_bindgen_test_distilled_lib_1_969c7d0305e0614c (void)) ("
  , "  a_type_t *arg1,"
  , "  uint32_t arg2,"
  , "  uint8_t arg3[]"
  , ")"
  , "{"
  , "  return &some_fun;"
  , "}"
  , "/* get_v_ptr */"
  , "__attribute__ ((const))"
  , "var_t *hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6 (void)"
  , "{"
  , "  return &v;"
  , "}"
  ]))

{-| __defined at:__ @distilled_lib_1.h:9:9@

    __exported by:__ @distilled_lib_1.h@
-}
data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: FC.CInt
    {- ^ __C declaration:__ @foo@

         __defined at:__ @distilled_lib_1.h:9:22@

         __exported by:__ @distilled_lib_1.h@
    -}
  , another_typedef_struct_t_bar :: FC.CChar
    {- ^ __C declaration:__ @bar@

         __defined at:__ @distilled_lib_1.h:9:32@

         __exported by:__ @distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Another_typedef_struct_t where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Another_typedef_struct_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_struct_t
            another_typedef_struct_t_foo2
            another_typedef_struct_t_bar3 ->
                 F.pokeByteOff ptr0 (0 :: Int) another_typedef_struct_t_foo2
              >> F.pokeByteOff ptr0 (4 :: Int) another_typedef_struct_t_bar3

{-| __defined at:__ @distilled_lib_1.h:10:9@

    __exported by:__ @distilled_lib_1.h@
-}
newtype Another_typedef_enum_e = Another_typedef_enum_e
  { un_Another_typedef_enum_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Another_typedef_enum_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Another_typedef_enum_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_enum_e un_Another_typedef_enum_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Another_typedef_enum_e2

instance HsBindgen.Runtime.CEnum.CEnum Another_typedef_enum_e where

  type CEnumZ Another_typedef_enum_e = FC.CUInt

  toCEnum = Another_typedef_enum_e

  fromCEnum = un_Another_typedef_enum_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "FOO")
                                                     , (1, Data.List.NonEmpty.singleton "BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Another_typedef_enum_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Another_typedef_enum_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Another_typedef_enum_e where

  minDeclaredValue = FOO

  maxDeclaredValue = BAR

instance Show Another_typedef_enum_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Another_typedef_enum_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @FOO@

    __defined at:__ @distilled_lib_1.h:10:16@

    __exported by:__ @distilled_lib_1.h@
-}
pattern FOO :: Another_typedef_enum_e
pattern FOO = Another_typedef_enum_e 0

{-| __C declaration:__ @BAR@

    __defined at:__ @distilled_lib_1.h:10:21@

    __exported by:__ @distilled_lib_1.h@
-}
pattern BAR :: Another_typedef_enum_e
pattern BAR = Another_typedef_enum_e 1

{-| __C declaration:__ @A@

    __defined at:__ @distilled_lib_1.h:11:9@

    __exported by:__ @distilled_lib_1.h@
-}
a :: FC.CInt
a = (5 :: FC.CInt)

{-| __C declaration:__ @B@

    __defined at:__ @distilled_lib_1.h:12:9@

    __exported by:__ @distilled_lib_1.h@
-}
b :: FC.CInt
b = (3 :: FC.CInt)

{-| __C declaration:__ @SOME_DEFINED_CONSTANT@

    __defined at:__ @distilled_lib_1.h:13:9@

    __exported by:__ @distilled_lib_1.h@
-}
sOME_DEFINED_CONSTANT :: FC.CInt
sOME_DEFINED_CONSTANT = (4 :: FC.CInt)

{-| __C declaration:__ @a_type_t@

    __defined at:__ @distilled_lib_1.h:14:13@

    __exported by:__ @distilled_lib_1.h@
-}
newtype A_type_t = A_type_t
  { un_A_type_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @var_t@

    __defined at:__ @distilled_lib_1.h:15:13@

    __exported by:__ @distilled_lib_1.h@
-}
newtype Var_t = Var_t
  { un_Var_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @a_typedef_struct_t@

    __defined at:__ @distilled_lib_1.h:35:16@

    __exported by:__ @distilled_lib_1.h@
-}
data A_typedef_struct_t = A_typedef_struct_t
  { a_typedef_struct_t_field_0 :: FC.CBool
    {- ^ __C declaration:__ @field_0@

         __defined at:__ @distilled_lib_1.h:37:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_1 :: HsBindgen.Runtime.Prelude.Word8
    {- ^ __C declaration:__ @field_1@

         __defined at:__ @distilled_lib_1.h:38:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_2 :: HsBindgen.Runtime.Prelude.Word16
    {- ^ __C declaration:__ @field_2@

         __defined at:__ @distilled_lib_1.h:39:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_3 :: HsBindgen.Runtime.Prelude.Word32
    {- ^ __C declaration:__ @field_3@

         __defined at:__ @distilled_lib_1.h:40:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_4 :: Another_typedef_struct_t
    {- ^ __C declaration:__ @field_4@

         __defined at:__ @distilled_lib_1.h:41:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_5 :: Ptr.Ptr Another_typedef_struct_t
    {- ^ __C declaration:__ @field_5@

         __defined at:__ @distilled_lib_1.h:42:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_6 :: Ptr.Ptr Void
    {- ^ __C declaration:__ @field_6@

         __defined at:__ @distilled_lib_1.h:43:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_7 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 7) HsBindgen.Runtime.Prelude.Word32
    {- ^ __C declaration:__ @field_7@

         __defined at:__ @distilled_lib_1.h:44:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_8 :: Another_typedef_enum_e
    {- ^ __C declaration:__ @field_8@

         __defined at:__ @distilled_lib_1.h:45:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_9 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) Another_typedef_enum_e
    {- ^ __C declaration:__ @field_9@

         __defined at:__ @distilled_lib_1.h:46:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  , a_typedef_struct_t_field_10 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) Another_typedef_enum_e)
    {- ^ __C declaration:__ @field_10@

         __defined at:__ @distilled_lib_1.h:47:31@

         __exported by:__ @distilled_lib_1.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable A_typedef_struct_t where

  sizeOf = \_ -> (140 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure A_typedef_struct_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (60 :: Int)
      <*> F.peekByteOff ptr0 (64 :: Int)
      <*> F.peekByteOff ptr0 (80 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_struct_t
            a_typedef_struct_t_field_02
            a_typedef_struct_t_field_13
            a_typedef_struct_t_field_24
            a_typedef_struct_t_field_35
            a_typedef_struct_t_field_46
            a_typedef_struct_t_field_57
            a_typedef_struct_t_field_68
            a_typedef_struct_t_field_79
            a_typedef_struct_t_field_810
            a_typedef_struct_t_field_911
            a_typedef_struct_t_field_1012 ->
                 F.pokeByteOff ptr0 (0 :: Int) a_typedef_struct_t_field_02
              >> F.pokeByteOff ptr0 (1 :: Int) a_typedef_struct_t_field_13
              >> F.pokeByteOff ptr0 (2 :: Int) a_typedef_struct_t_field_24
              >> F.pokeByteOff ptr0 (4 :: Int) a_typedef_struct_t_field_35
              >> F.pokeByteOff ptr0 (8 :: Int) a_typedef_struct_t_field_46
              >> F.pokeByteOff ptr0 (16 :: Int) a_typedef_struct_t_field_57
              >> F.pokeByteOff ptr0 (24 :: Int) a_typedef_struct_t_field_68
              >> F.pokeByteOff ptr0 (32 :: Int) a_typedef_struct_t_field_79
              >> F.pokeByteOff ptr0 (60 :: Int) a_typedef_struct_t_field_810
              >> F.pokeByteOff ptr0 (64 :: Int) a_typedef_struct_t_field_911
              >> F.pokeByteOff ptr0 (80 :: Int) a_typedef_struct_t_field_1012

{-| __C declaration:__ @A_DEFINE_0@

    __defined at:__ @distilled_lib_1.h:53:9@

    __exported by:__ @distilled_lib_1.h@
-}
a_DEFINE_0 :: FC.CInt
a_DEFINE_0 = (0 :: FC.CInt)

{-| __C declaration:__ @A_DEFINE_1@

    __defined at:__ @distilled_lib_1.h:54:9@

    __exported by:__ @distilled_lib_1.h@
-}
a_DEFINE_1 :: FC.CUInt
a_DEFINE_1 = (20560 :: FC.CUInt)

{-| __C declaration:__ @A_DEFINE_2@

    __defined at:__ @distilled_lib_1.h:55:9@

    __exported by:__ @distilled_lib_1.h@
-}
a_DEFINE_2 :: FC.CInt
a_DEFINE_2 = (2 :: FC.CInt)

{-| __C declaration:__ @TWO_ARGS@

    __defined at:__ @distilled_lib_1.h:56:9@

    __exported by:__ @distilled_lib_1.h@
-}
tWO_ARGS :: ((,) FC.CInt) FC.CInt
tWO_ARGS = (,) (13398 :: FC.CInt) (30874 :: FC.CInt)

{-| __defined at:__ @distilled_lib_1.h:61:9@

    __exported by:__ @distilled_lib_1.h@
-}
newtype A_typedef_enum_e = A_typedef_enum_e
  { un_A_typedef_enum_e :: FC.CUChar
  }
  deriving stock (Eq, Ord)

instance F.Storable A_typedef_enum_e where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure A_typedef_enum_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_enum_e un_A_typedef_enum_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_A_typedef_enum_e2

instance HsBindgen.Runtime.CEnum.CEnum A_typedef_enum_e where

  type CEnumZ A_typedef_enum_e = FC.CUChar

  toCEnum = A_typedef_enum_e

  fromCEnum = un_A_typedef_enum_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "ENUM_CASE_0")
                                                     , (1, Data.List.NonEmpty.singleton "ENUM_CASE_1")
                                                     , (2, Data.List.NonEmpty.singleton "ENUM_CASE_2")
                                                     , (3, Data.List.NonEmpty.singleton "ENUM_CASE_3")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "A_typedef_enum_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "A_typedef_enum_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum A_typedef_enum_e where

  minDeclaredValue = ENUM_CASE_0

  maxDeclaredValue = ENUM_CASE_3

instance Show A_typedef_enum_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read A_typedef_enum_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @ENUM_CASE_0@

    __defined at:__ @distilled_lib_1.h:63:3@

    __exported by:__ @distilled_lib_1.h@
-}
pattern ENUM_CASE_0 :: A_typedef_enum_e
pattern ENUM_CASE_0 = A_typedef_enum_e 0

{-| __C declaration:__ @ENUM_CASE_1@

    __defined at:__ @distilled_lib_1.h:64:3@

    __exported by:__ @distilled_lib_1.h@
-}
pattern ENUM_CASE_1 :: A_typedef_enum_e
pattern ENUM_CASE_1 = A_typedef_enum_e 1

{-| __C declaration:__ @ENUM_CASE_2@

    __defined at:__ @distilled_lib_1.h:65:3@

    __exported by:__ @distilled_lib_1.h@
-}
pattern ENUM_CASE_2 :: A_typedef_enum_e
pattern ENUM_CASE_2 = A_typedef_enum_e 2

{-| __C declaration:__ @ENUM_CASE_3@

    __defined at:__ @distilled_lib_1.h:66:3@

    __exported by:__ @distilled_lib_1.h@
-}
pattern ENUM_CASE_3 :: A_typedef_enum_e
pattern ENUM_CASE_3 = A_typedef_enum_e 3

{-| Auxiliary type used by 'Callback_t'

__defined at:__ @distilled_lib_1.h:77:19@

__exported by:__ @distilled_lib_1.h@
-}
newtype Callback_t_Deref = Callback_t_Deref
  { un_Callback_t_Deref :: (Ptr.Ptr Void) -> HsBindgen.Runtime.Prelude.Word32 -> IO HsBindgen.Runtime.Prelude.Word32
  }

foreign import ccall safe "wrapper" toCallback_t_Deref ::
     Callback_t_Deref
  -> IO (Ptr.FunPtr Callback_t_Deref)

foreign import ccall safe "dynamic" fromCallback_t_Deref ::
     Ptr.FunPtr Callback_t_Deref
  -> Callback_t_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Callback_t_Deref where

  toFunPtr = toCallback_t_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Callback_t_Deref where

  fromFunPtr = fromCallback_t_Deref

{-| __C declaration:__ @callback_t@

    __defined at:__ @distilled_lib_1.h:77:19@

    __exported by:__ @distilled_lib_1.h@
-}
newtype Callback_t = Callback_t
  { un_Callback_t :: Ptr.FunPtr Callback_t_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @some_fun@

    __defined at:__ @distilled_lib_1.h:72:9@

    __exported by:__ @distilled_lib_1.h@
-}
foreign import ccall safe "hs_bindgen_test_distilled_lib_1_29c178c31334688f" some_fun ::
     Ptr.Ptr A_type_t
     {- ^ __C declaration:__ @i@
     -}
  -> HsBindgen.Runtime.Prelude.Word32
     {- ^ __C declaration:__ @j@
     -}
  -> Ptr.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __C declaration:__ @k@
     -}
  -> IO HsBindgen.Runtime.Prelude.Int32

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_969c7d0305e0614c" hs_bindgen_test_distilled_lib_1_969c7d0305e0614c ::
     IO (Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32))

{-# NOINLINE some_fun_ptr #-}

{-| __C declaration:__ @some_fun@

    __defined at:__ @distilled_lib_1.h:72:9@

    __exported by:__ @distilled_lib_1.h@
-}
some_fun_ptr :: Ptr.FunPtr ((Ptr.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32)
some_fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_969c7d0305e0614c

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6" hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6 ::
     IO (Ptr.Ptr Var_t)

{-# NOINLINE v_ptr #-}

{-| __C declaration:__ @v@

    __defined at:__ @distilled_lib_1.h:91:14@

    __exported by:__ @distilled_lib_1.h@
-}
v_ptr :: Ptr.Ptr Var_t
v_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_b9e65c51f976c6f6
