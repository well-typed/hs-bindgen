{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.ConstantArray
import Prelude ((<*>), (>>), IO, pure)
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

foreign import capi safe "distilled_lib_1.h some_fun" some_fun :: (F.Ptr CATypeT) -> CUint32T -> Void -> IO CInt32T

data CAnotherTypedefStructT = MkCAnotherTypedefStructT
  { cAnotherTypedefStructT_foo :: FC.CInt
  , cAnotherTypedefStructT_bar :: FC.CChar
  }

instance F.Storable CAnotherTypedefStructT where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCAnotherTypedefStructT
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCAnotherTypedefStructT cAnotherTypedefStructT_foo2 cAnotherTypedefStructT_bar3 ->
               F.pokeByteOff ptr0 0 cAnotherTypedefStructT_foo2
            >> F.pokeByteOff ptr0 4 cAnotherTypedefStructT_bar3

newtype CAnotherTypedefEnumE = MkCAnotherTypedefEnumE
  { unCAnotherTypedefEnumE :: FC.CUInt
  }

instance F.Storable CAnotherTypedefEnumE where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCAnotherTypedefEnumE
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCAnotherTypedefEnumE unCAnotherTypedefEnumE2 ->
            F.pokeByteOff ptr0 0 unCAnotherTypedefEnumE2

pattern MkCFOO :: CAnotherTypedefEnumE
pattern MkCFOO = MkCAnotherTypedefEnumE 0

pattern MkCBAR :: CAnotherTypedefEnumE
pattern MkCBAR = MkCAnotherTypedefEnumE 1

newtype CATypeT = MkCATypeT
  { unCATypeT :: FC.CInt
  }

deriving newtype instance F.Storable CATypeT

newtype CVarT = MkCVarT
  { unCVarT :: FC.CInt
  }

deriving newtype instance F.Storable CVarT

newtype CUint8T = MkCUint8T
  { unCUint8T :: FC.CSChar
  }

deriving newtype instance F.Storable CUint8T

newtype CUint16T = MkCUint16T
  { unCUint16T :: FC.CUShort
  }

deriving newtype instance F.Storable CUint16T

newtype CUint32T = MkCUint32T
  { unCUint32T :: FC.CUInt
  }

deriving newtype instance F.Storable CUint32T

data CATypedefStruct = MkCATypedefStruct
  { cATypedefStruct_field_0 :: FC.CBool
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
  { unCATypedefStructT :: CATypedefStruct
  }

deriving newtype instance F.Storable CATypedefStructT

newtype CATypedefEnumE = MkCATypedefEnumE
  { unCATypedefEnumE :: FC.CSChar
  }

instance F.Storable CATypedefEnumE where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCATypedefEnumE
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCATypedefEnumE unCATypedefEnumE2 -> F.pokeByteOff ptr0 0 unCATypedefEnumE2

pattern MkCENUMCASE0 :: CATypedefEnumE
pattern MkCENUMCASE0 = MkCATypedefEnumE 0

pattern MkCENUMCASE1 :: CATypedefEnumE
pattern MkCENUMCASE1 = MkCATypedefEnumE 1

pattern MkCENUMCASE2 :: CATypedefEnumE
pattern MkCENUMCASE2 = MkCATypedefEnumE 2

pattern MkCENUMCASE3 :: CATypedefEnumE
pattern MkCENUMCASE3 = MkCATypedefEnumE 3

newtype CInt32T = MkCInt32T
  { unCInt32T :: FC.CInt
  }

deriving newtype instance F.Storable CInt32T

newtype CCallbackT = MkCCallbackT
  { unCCallbackT :: F.FunPtr ((F.Ptr Void) -> CUint32T -> IO CUint32T)
  }

deriving newtype instance F.Storable CCallbackT
