{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Patterns.FlexibleArrayMember
import Prelude ((<*>), (>>), Show, pure)

data Pascal = Pascal
  { pascal_len :: FC.CInt
  }

instance F.Storable Pascal where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Pascal
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 -> F.pokeByteOff ptr0 0 pascal_len2

deriving stock instance Show Pascal

instance HsBindgen.Patterns.FlexibleArrayMember.HasFlexibleArrayMember Pascal FC.CChar where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
  , foo_bar_y :: FC.CInt
  }

instance F.Storable Foo_bar where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo_bar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               F.pokeByteOff ptr0 0 foo_bar_x2
            >> F.pokeByteOff ptr0 4 foo_bar_y3

deriving stock instance Show Foo_bar

data Foo = Foo
  { foo_len :: FC.CInt
  }

instance F.Storable Foo where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 -> F.pokeByteOff ptr0 0 foo_len2

deriving stock instance Show Foo

instance HsBindgen.Patterns.FlexibleArrayMember.HasFlexibleArrayMember Foo Foo_bar where

  flexibleArrayMemberOffset = \_ty0 -> 4

data Diff = Diff
  { diff_first :: FC.CLong
  , diff_second :: FC.CChar
  }

instance F.Storable Diff where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Diff
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               F.pokeByteOff ptr0 0 diff_first2
            >> F.pokeByteOff ptr0 8 diff_second3

deriving stock instance Show Diff

instance HsBindgen.Patterns.FlexibleArrayMember.HasFlexibleArrayMember Diff FC.CChar where

  flexibleArrayMemberOffset = \_ty0 -> 9
