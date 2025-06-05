{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.FlexibleArrayMember
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Pascal = Pascal
  { pascal_len :: FC.CInt
  }

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
          Pascal pascal_len2 -> F.pokeByteOff ptr0 (0 :: Int) pascal_len2

deriving stock instance Show Pascal

deriving stock instance Eq Pascal

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CChar Pascal where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
  , foo_bar_y :: FC.CInt
  }

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

deriving stock instance Show Foo_bar

deriving stock instance Eq Foo_bar

data Foo = Foo
  { foo_len :: FC.CInt
  }

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
          Foo foo_len2 -> F.pokeByteOff ptr0 (0 :: Int) foo_len2

deriving stock instance Show Foo

deriving stock instance Eq Foo

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember Foo_bar Foo where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Diff = Diff
  { diff_first :: FC.CLong
  , diff_second :: FC.CChar
  }

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

deriving stock instance Show Diff

deriving stock instance Eq Diff

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CChar Diff where

  flexibleArrayMemberOffset = \_ty0 -> 9
