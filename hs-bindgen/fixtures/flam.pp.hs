{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FlexibleArrayMember
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Pascal = Pascal
  { pascal_len :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Pascal where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Pascal
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 ->
            F.pokeByteOff ptr0 (0 :: Int) pascal_len2

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CChar Pascal where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
  , foo_bar_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Foo_bar where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo_bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_bar_x2
            >> F.pokeByteOff ptr0 (4 :: Int) foo_bar_y3

data Foo = Foo
  { foo_len :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 ->
            F.pokeByteOff ptr0 (0 :: Int) foo_len2

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember Foo_bar Foo where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Diff = Diff
  { diff_first :: FC.CLong
  , diff_second :: FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable Diff where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Diff
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               F.pokeByteOff ptr0 (0 :: Int) diff_first2
            >> F.pokeByteOff ptr0 (8 :: Int) diff_second3

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CChar Diff where

  flexibleArrayMemberOffset = \_ty0 -> 9

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

  __from C:__ @triplets@
-}
data Triplets = Triplets
  { triplets_len :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Triplets where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Triplets
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triplets triplets_len2 ->
            F.pokeByteOff ptr0 (0 :: Int) triplets_len2

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) Triplets where

  flexibleArrayMemberOffset = \_ty0 -> 4
