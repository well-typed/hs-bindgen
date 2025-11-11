{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FunPtr
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.SizedByteArray
import qualified Text.Read
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return, showsPrec)

{-| __C declaration:__ @A@

    __defined at:__ @reparse.h:3:9@

    __exported by:__ @reparse.h@
-}
newtype A = A
  { un_A :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @some_struct@

    __defined at:__ @reparse.h:7:8@

    __exported by:__ @reparse.h@
-}
data Some_struct = Some_struct
  {}
  deriving stock (Eq, Show)

instance F.Storable Some_struct where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Some_struct

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_struct -> return ()

{-| __C declaration:__ @some_union@

    __defined at:__ @reparse.h:8:7@

    __exported by:__ @reparse.h@
-}
newtype Some_union = Some_union
  { un_Some_union :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 0) 1 instance F.Storable Some_union

{-| __C declaration:__ @some_enum@

    __defined at:__ @reparse.h:9:6@

    __exported by:__ @reparse.h@
-}
newtype Some_enum = Some_enum
  { un_Some_enum :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Some_enum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Some_enum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Some_enum un_Some_enum2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Some_enum2

instance HsBindgen.Runtime.CEnum.CEnum Some_enum where

  type CEnumZ Some_enum = FC.CUInt

  toCEnum = Some_enum

  fromCEnum = un_Some_enum

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "ENUM_A")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Some_enum"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Some_enum"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Some_enum where

  minDeclaredValue = ENUM_A

  maxDeclaredValue = ENUM_A

instance Show Some_enum where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Some_enum where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

{-| __C declaration:__ @ENUM_A@

    __defined at:__ @reparse.h:9:18@

    __exported by:__ @reparse.h@
-}
pattern ENUM_A :: Some_enum
pattern ENUM_A = Some_enum 0

{-| __C declaration:__ @arr_typedef1@

    __defined at:__ @reparse.h:109:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef1 = Arr_typedef1
  { un_Arr_typedef1 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray A
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @arr_typedef2@

    __defined at:__ @reparse.h:110:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef2 = Arr_typedef2
  { un_Arr_typedef2 :: HsBindgen.Runtime.IncompleteArray.IncompleteArray (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @arr_typedef3@

    __defined at:__ @reparse.h:111:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef3 = Arr_typedef3
  { un_Arr_typedef3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) A
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @arr_typedef4@

    __defined at:__ @reparse.h:112:13@

    __exported by:__ @reparse.h@
-}
newtype Arr_typedef4 = Arr_typedef4
  { un_Arr_typedef4 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) (Ptr.Ptr A)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| Typedefs

__C declaration:__ @typedef1@

__defined at:__ @reparse.h:118:14@

__exported by:__ @reparse.h@
-}
newtype Typedef1 = Typedef1
  { un_Typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @typedef2@

    __defined at:__ @reparse.h:119:14@

    __exported by:__ @reparse.h@
-}
newtype Typedef2 = Typedef2
  { un_Typedef2 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @typedef3@

    __defined at:__ @reparse.h:120:14@

    __exported by:__ @reparse.h@
-}
newtype Typedef3 = Typedef3
  { un_Typedef3 :: Ptr.Ptr (Ptr.Ptr A)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef1'

__defined at:__ @reparse.h:132:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef1_Deref = Funptr_typedef1_Deref
  { un_Funptr_typedef1_Deref :: IO A
  }

foreign import ccall safe "wrapper" toFunptr_typedef1_Deref ::
     Funptr_typedef1_Deref
  -> IO (Ptr.FunPtr Funptr_typedef1_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef1_Deref ::
     Ptr.FunPtr Funptr_typedef1_Deref
  -> Funptr_typedef1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef1_Deref where

  toFunPtr = toFunptr_typedef1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef1_Deref where

  fromFunPtr = fromFunptr_typedef1_Deref

{-| __C declaration:__ @funptr_typedef1@

    __defined at:__ @reparse.h:132:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef1 = Funptr_typedef1
  { un_Funptr_typedef1 :: Ptr.FunPtr Funptr_typedef1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef2'

__defined at:__ @reparse.h:133:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef2_Deref = Funptr_typedef2_Deref
  { un_Funptr_typedef2_Deref :: IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toFunptr_typedef2_Deref ::
     Funptr_typedef2_Deref
  -> IO (Ptr.FunPtr Funptr_typedef2_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef2_Deref ::
     Ptr.FunPtr Funptr_typedef2_Deref
  -> Funptr_typedef2_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef2_Deref where

  toFunPtr = toFunptr_typedef2_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef2_Deref where

  fromFunPtr = fromFunptr_typedef2_Deref

{-| __C declaration:__ @funptr_typedef2@

    __defined at:__ @reparse.h:133:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef2 = Funptr_typedef2
  { un_Funptr_typedef2 :: Ptr.FunPtr Funptr_typedef2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef3'

__defined at:__ @reparse.h:134:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef3_Deref = Funptr_typedef3_Deref
  { un_Funptr_typedef3_Deref :: IO (Ptr.Ptr (Ptr.Ptr A))
  }

foreign import ccall safe "wrapper" toFunptr_typedef3_Deref ::
     Funptr_typedef3_Deref
  -> IO (Ptr.FunPtr Funptr_typedef3_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef3_Deref ::
     Ptr.FunPtr Funptr_typedef3_Deref
  -> Funptr_typedef3_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef3_Deref where

  toFunPtr = toFunptr_typedef3_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef3_Deref where

  fromFunPtr = fromFunptr_typedef3_Deref

{-| __C declaration:__ @funptr_typedef3@

    __defined at:__ @reparse.h:134:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef3 = Funptr_typedef3
  { un_Funptr_typedef3 :: Ptr.FunPtr Funptr_typedef3_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef4'

__defined at:__ @reparse.h:135:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef4_Deref = Funptr_typedef4_Deref
  { un_Funptr_typedef4_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toFunptr_typedef4_Deref ::
     Funptr_typedef4_Deref
  -> IO (Ptr.FunPtr Funptr_typedef4_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef4_Deref ::
     Ptr.FunPtr Funptr_typedef4_Deref
  -> Funptr_typedef4_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef4_Deref where

  toFunPtr = toFunptr_typedef4_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef4_Deref where

  fromFunPtr = fromFunptr_typedef4_Deref

{-| __C declaration:__ @funptr_typedef4@

    __defined at:__ @reparse.h:135:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef4 = Funptr_typedef4
  { un_Funptr_typedef4 :: Ptr.FunPtr Funptr_typedef4_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Funptr_typedef5'

__defined at:__ @reparse.h:136:16@

__exported by:__ @reparse.h@
-}
newtype Funptr_typedef5_Deref = Funptr_typedef5_Deref
  { un_Funptr_typedef5_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toFunptr_typedef5_Deref ::
     Funptr_typedef5_Deref
  -> IO (Ptr.FunPtr Funptr_typedef5_Deref)

foreign import ccall safe "dynamic" fromFunptr_typedef5_Deref ::
     Ptr.FunPtr Funptr_typedef5_Deref
  -> Funptr_typedef5_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Funptr_typedef5_Deref where

  toFunPtr = toFunptr_typedef5_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Funptr_typedef5_Deref where

  fromFunPtr = fromFunptr_typedef5_Deref

{-| __C declaration:__ @funptr_typedef5@

    __defined at:__ @reparse.h:136:16@

    __exported by:__ @reparse.h@
-}
newtype Funptr_typedef5 = Funptr_typedef5
  { un_Funptr_typedef5 :: Ptr.FunPtr Funptr_typedef5_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @comments2@

    __defined at:__ @reparse.h:145:30@

    __exported by:__ @reparse.h@
-}
newtype Comments2 = Comments2
  { un_Comments2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| Struct fields

__C declaration:__ @example_struct@

__defined at:__ @reparse.h:151:8@

__exported by:__ @reparse.h@
-}
data Example_struct = Example_struct
  { example_struct_field1 :: A
    {- ^ __C declaration:__ @field1@

         __defined at:__ @reparse.h:152:8@

         __exported by:__ @reparse.h@
    -}
  , example_struct_field2 :: Ptr.Ptr A
    {- ^ __C declaration:__ @field2@

         __defined at:__ @reparse.h:153:8@

         __exported by:__ @reparse.h@
    -}
  , example_struct_field3 :: Ptr.Ptr (Ptr.Ptr A)
    {- ^ __C declaration:__ @field3@

         __defined at:__ @reparse.h:154:8@

         __exported by:__ @reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example_struct where

  sizeOf = \_ -> (24 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct
            example_struct_field12
            example_struct_field23
            example_struct_field34 ->
                 F.pokeByteOff ptr0 (0 :: Int) example_struct_field12
              >> F.pokeByteOff ptr0 (8 :: Int) example_struct_field23
              >> F.pokeByteOff ptr0 (16 :: Int) example_struct_field34

{-| __C declaration:__ @const_typedef1@

    __defined at:__ @reparse.h:220:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef1 = Const_typedef1
  { un_Const_typedef1 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @const_typedef2@

    __defined at:__ @reparse.h:221:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef2 = Const_typedef2
  { un_Const_typedef2 :: A
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @const_typedef3@

    __defined at:__ @reparse.h:222:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef3 = Const_typedef3
  { un_Const_typedef3 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef4@

    __defined at:__ @reparse.h:223:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef4 = Const_typedef4
  { un_Const_typedef4 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef5@

    __defined at:__ @reparse.h:224:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef5 = Const_typedef5
  { un_Const_typedef5 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef6@

    __defined at:__ @reparse.h:225:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef6 = Const_typedef6
  { un_Const_typedef6 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @const_typedef7@

    __defined at:__ @reparse.h:226:25@

    __exported by:__ @reparse.h@
-}
newtype Const_typedef7 = Const_typedef7
  { un_Const_typedef7 :: Ptr.Ptr A
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @example_struct_with_const@

    __defined at:__ @reparse.h:228:8@

    __exported by:__ @reparse.h@
-}
data Example_struct_with_const = Example_struct_with_const
  { example_struct_with_const_const_field1 :: A
    {- ^ __C declaration:__ @const_field1@

         __defined at:__ @reparse.h:229:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field2 :: A
    {- ^ __C declaration:__ @const_field2@

         __defined at:__ @reparse.h:230:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field3 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field3@

         __defined at:__ @reparse.h:231:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field4 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field4@

         __defined at:__ @reparse.h:232:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field5 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field5@

         __defined at:__ @reparse.h:233:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field6 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field6@

         __defined at:__ @reparse.h:234:19@

         __exported by:__ @reparse.h@
    -}
  , example_struct_with_const_const_field7 :: Ptr.Ptr A
    {- ^ __C declaration:__ @const_field7@

         __defined at:__ @reparse.h:235:19@

         __exported by:__ @reparse.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example_struct_with_const where

  sizeOf = \_ -> (48 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Example_struct_with_const
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (40 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example_struct_with_const
            example_struct_with_const_const_field12
            example_struct_with_const_const_field23
            example_struct_with_const_const_field34
            example_struct_with_const_const_field45
            example_struct_with_const_const_field56
            example_struct_with_const_const_field67
            example_struct_with_const_const_field78 ->
                 F.pokeByteOff ptr0 (0 :: Int) example_struct_with_const_const_field12
              >> F.pokeByteOff ptr0 (4 :: Int) example_struct_with_const_const_field23
              >> F.pokeByteOff ptr0 (8 :: Int) example_struct_with_const_const_field34
              >> F.pokeByteOff ptr0 (16 :: Int) example_struct_with_const_const_field45
              >> F.pokeByteOff ptr0 (24 :: Int) example_struct_with_const_const_field56
              >> F.pokeByteOff ptr0 (32 :: Int) example_struct_with_const_const_field67
              >> F.pokeByteOff ptr0 (40 :: Int) example_struct_with_const_const_field78

{-| Auxiliary type used by 'Const_funptr1'

__defined at:__ @reparse.h:238:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr1_Deref = Const_funptr1_Deref
  { un_Const_funptr1_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toConst_funptr1_Deref ::
     Const_funptr1_Deref
  -> IO (Ptr.FunPtr Const_funptr1_Deref)

foreign import ccall safe "dynamic" fromConst_funptr1_Deref ::
     Ptr.FunPtr Const_funptr1_Deref
  -> Const_funptr1_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr1_Deref where

  toFunPtr = toConst_funptr1_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr1_Deref where

  fromFunPtr = fromConst_funptr1_Deref

{-| __C declaration:__ @const_funptr1@

    __defined at:__ @reparse.h:238:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr1 = Const_funptr1
  { un_Const_funptr1 :: Ptr.FunPtr Const_funptr1_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr2'

__defined at:__ @reparse.h:239:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr2_Deref = Const_funptr2_Deref
  { un_Const_funptr2_Deref :: FC.CInt -> FC.CDouble -> IO A
  }

foreign import ccall safe "wrapper" toConst_funptr2_Deref ::
     Const_funptr2_Deref
  -> IO (Ptr.FunPtr Const_funptr2_Deref)

foreign import ccall safe "dynamic" fromConst_funptr2_Deref ::
     Ptr.FunPtr Const_funptr2_Deref
  -> Const_funptr2_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr2_Deref where

  toFunPtr = toConst_funptr2_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr2_Deref where

  fromFunPtr = fromConst_funptr2_Deref

{-| __C declaration:__ @const_funptr2@

    __defined at:__ @reparse.h:239:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr2 = Const_funptr2
  { un_Const_funptr2 :: Ptr.FunPtr Const_funptr2_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr3'

__defined at:__ @reparse.h:240:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr3_Deref = Const_funptr3_Deref
  { un_Const_funptr3_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr3_Deref ::
     Const_funptr3_Deref
  -> IO (Ptr.FunPtr Const_funptr3_Deref)

foreign import ccall safe "dynamic" fromConst_funptr3_Deref ::
     Ptr.FunPtr Const_funptr3_Deref
  -> Const_funptr3_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr3_Deref where

  toFunPtr = toConst_funptr3_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr3_Deref where

  fromFunPtr = fromConst_funptr3_Deref

{-| __C declaration:__ @const_funptr3@

    __defined at:__ @reparse.h:240:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr3 = Const_funptr3
  { un_Const_funptr3 :: Ptr.FunPtr Const_funptr3_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr4'

__defined at:__ @reparse.h:241:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr4_Deref = Const_funptr4_Deref
  { un_Const_funptr4_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr4_Deref ::
     Const_funptr4_Deref
  -> IO (Ptr.FunPtr Const_funptr4_Deref)

foreign import ccall safe "dynamic" fromConst_funptr4_Deref ::
     Ptr.FunPtr Const_funptr4_Deref
  -> Const_funptr4_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr4_Deref where

  toFunPtr = toConst_funptr4_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr4_Deref where

  fromFunPtr = fromConst_funptr4_Deref

{-| __C declaration:__ @const_funptr4@

    __defined at:__ @reparse.h:241:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr4 = Const_funptr4
  { un_Const_funptr4 :: Ptr.FunPtr Const_funptr4_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr5'

__defined at:__ @reparse.h:242:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr5_Deref = Const_funptr5_Deref
  { un_Const_funptr5_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr5_Deref ::
     Const_funptr5_Deref
  -> IO (Ptr.FunPtr Const_funptr5_Deref)

foreign import ccall safe "dynamic" fromConst_funptr5_Deref ::
     Ptr.FunPtr Const_funptr5_Deref
  -> Const_funptr5_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr5_Deref where

  toFunPtr = toConst_funptr5_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr5_Deref where

  fromFunPtr = fromConst_funptr5_Deref

{-| __C declaration:__ @const_funptr5@

    __defined at:__ @reparse.h:242:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr5 = Const_funptr5
  { un_Const_funptr5 :: Ptr.FunPtr Const_funptr5_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr6'

__defined at:__ @reparse.h:243:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr6_Deref = Const_funptr6_Deref
  { un_Const_funptr6_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr6_Deref ::
     Const_funptr6_Deref
  -> IO (Ptr.FunPtr Const_funptr6_Deref)

foreign import ccall safe "dynamic" fromConst_funptr6_Deref ::
     Ptr.FunPtr Const_funptr6_Deref
  -> Const_funptr6_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr6_Deref where

  toFunPtr = toConst_funptr6_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr6_Deref where

  fromFunPtr = fromConst_funptr6_Deref

{-| __C declaration:__ @const_funptr6@

    __defined at:__ @reparse.h:243:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr6 = Const_funptr6
  { un_Const_funptr6 :: Ptr.FunPtr Const_funptr6_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| Auxiliary type used by 'Const_funptr7'

__defined at:__ @reparse.h:244:27@

__exported by:__ @reparse.h@
-}
newtype Const_funptr7_Deref = Const_funptr7_Deref
  { un_Const_funptr7_Deref :: FC.CInt -> FC.CDouble -> IO (Ptr.Ptr A)
  }

foreign import ccall safe "wrapper" toConst_funptr7_Deref ::
     Const_funptr7_Deref
  -> IO (Ptr.FunPtr Const_funptr7_Deref)

foreign import ccall safe "dynamic" fromConst_funptr7_Deref ::
     Ptr.FunPtr Const_funptr7_Deref
  -> Const_funptr7_Deref

instance HsBindgen.Runtime.FunPtr.ToFunPtr Const_funptr7_Deref where

  toFunPtr = toConst_funptr7_Deref

instance HsBindgen.Runtime.FunPtr.FromFunPtr Const_funptr7_Deref where

  fromFunPtr = fromConst_funptr7_Deref

{-| __C declaration:__ @const_funptr7@

    __defined at:__ @reparse.h:244:27@

    __exported by:__ @reparse.h@
-}
newtype Const_funptr7 = Const_funptr7
  { un_Const_funptr7 :: Ptr.FunPtr Const_funptr7_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @BOOL@

    __defined at:__ @reparse.h:280:9@

    __exported by:__ @reparse.h@
-}
newtype BOOL = BOOL
  { un_BOOL :: FC.CBool
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INT@

    __defined at:__ @reparse.h:281:9@

    __exported by:__ @reparse.h@
-}
newtype INT = INT
  { un_INT :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @INTP@

    __defined at:__ @reparse.h:282:9@

    __exported by:__ @reparse.h@
-}
newtype INTP = INTP
  { un_INTP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @INTCP@

    __defined at:__ @reparse.h:283:9@

    __exported by:__ @reparse.h@
-}
newtype INTCP = INTCP
  { un_INTCP :: Ptr.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
