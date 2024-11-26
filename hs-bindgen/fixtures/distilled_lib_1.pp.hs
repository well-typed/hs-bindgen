{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.ConstantArray
import Prelude ((<*>), (>>), pure)
import qualified Prelude as P

a :: forall a0. P.Integral a0 => a0
a = 5

b :: forall a0. P.Integral a0 => a0
b = 3

sOME_DEFINED_CONSTANT :: forall a0. P.Integral a0 => a0
sOME_DEFINED_CONSTANT = 4

a_DEFINE_0 :: forall a0. P.Integral a0 => a0
a_DEFINE_0 = 0

a_DEFINE_1 :: FC.CUInt
a_DEFINE_1 = 20560

a_DEFINE_2 :: forall a0. P.Integral a0 => a0
a_DEFINE_2 = 2

tWO_ARGS :: forall a0. P.Integral a0 => a0
tWO_ARGS = 13398

data CX = MkCX
  { cX_foo :: FC.CInt
  , cX_bar :: FC.CChar
  }

instance F.Storable CX where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCX
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCX cX_foo2 cX_bar3 ->
               F.pokeByteOff ptr0 0 cX_foo2
            >> F.pokeByteOff ptr0 4 cX_bar3

newtype CAnotherTypedefStructT = MkCAnotherTypedefStructT
  { unCAnotherTypedefStructT :: CStruct'0020anotherTypedefStructT
  }

deriving newtype instance F.Storable CAnotherTypedefStructT

newtype CX = MkCX
  { unCX :: FC.CUInt
  }

instance F.Storable CX where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCX
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCX unCX2 -> F.pokeByteOff ptr0 0 unCX2

newtype CAnotherTypedefEnumE = MkCAnotherTypedefEnumE
  { unCAnotherTypedefEnumE :: CEnum'0020anotherTypedefEnumE
  }

deriving newtype instance F.Storable CAnotherTypedefEnumE

newtype CATypeT = MkCATypeT
  { unCATypeT :: FC.CInt
  }

deriving newtype instance F.Storable CATypeT

newtype CVarT = MkCVarT
  { unCVarT :: FC.CInt
  }

deriving newtype instance F.Storable CVarT

data CATypedefStruct = MkCATypedefStruct
  { cATypedefStruct_field_0 :: CBool'
  , cATypedefStruct_field_1 :: CUint8T
  , cATypedefStruct_field_2 :: CUint16T
  , cATypedefStruct_field_3 :: CUint32T
  , cATypedefStruct_field_4 :: CAnotherTypedefStructT
  , cATypedefStruct_field_5 :: F.Ptr CAnotherTypedefStructT
  , cATypedefStruct_field_6 :: F.Ptr Void
  , cATypedefStruct_field_7 :: (HsBindgen.ConstantArray.ConstantArray 7) CUint32T
  , cATypedefStruct_field_8 :: CAnotherTypedefEnumE
  , cATypedefStruct_field_9 :: CAnotherTypedefEnumE
  , cATypedefStruct_field_10 :: CAnotherTypedefEnumE
  }

instance F.Storable CATypedefStruct where

  sizeOf = \_ -> 140

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCATypedefStruct
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1
      <*> F.peekByteOff ptr0 2
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8
      <*> F.peekByteOff ptr0 16
      <*> F.peekByteOff ptr0 24
      <*> F.peekByteOff ptr0 32
      <*> F.peekByteOff ptr0 60
      <*> F.peekByteOff ptr0 64
      <*> F.peekByteOff ptr0 80

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCATypedefStruct
            cATypedefStruct_field_02
            cATypedefStruct_field_13
            cATypedefStruct_field_24
            cATypedefStruct_field_35
            cATypedefStruct_field_46
            cATypedefStruct_field_57
            cATypedefStruct_field_68
            cATypedefStruct_field_79
            cATypedefStruct_field_810
            cATypedefStruct_field_911
            cATypedefStruct_field_1012 ->
                 F.pokeByteOff ptr0 0 cATypedefStruct_field_02
              >> F.pokeByteOff ptr0 1 cATypedefStruct_field_13
              >> F.pokeByteOff ptr0 2 cATypedefStruct_field_24
              >> F.pokeByteOff ptr0 4 cATypedefStruct_field_35
              >> F.pokeByteOff ptr0 8 cATypedefStruct_field_46
              >> F.pokeByteOff ptr0 16 cATypedefStruct_field_57
              >> F.pokeByteOff ptr0 24 cATypedefStruct_field_68
              >> F.pokeByteOff ptr0 32 cATypedefStruct_field_79
              >> F.pokeByteOff ptr0 60 cATypedefStruct_field_810
              >> F.pokeByteOff ptr0 64 cATypedefStruct_field_911
              >> F.pokeByteOff ptr0 80 cATypedefStruct_field_1012

newtype CATypedefStructT = MkCATypedefStructT
  { unCATypedefStructT :: CStruct'0020aTypedefStruct
  }

deriving newtype instance F.Storable CATypedefStructT

newtype CX = MkCX
  { unCX :: FC.CSChar
  }

instance F.Storable CX where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCX
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCX unCX2 -> F.pokeByteOff ptr0 0 unCX2

newtype CATypedefEnumE = MkCATypedefEnumE
  { unCATypedefEnumE :: CEnum'0020aTypedefEnumE
  }

deriving newtype instance F.Storable CATypedefEnumE

newtype CCallbackT = MkCCallbackT
  { unCCallbackT :: F.Ptr Void
  }

deriving newtype instance F.Storable CCallbackT
