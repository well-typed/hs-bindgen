{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Show, pure)

newtype Uint64_t = Uint64_t
  { unUint64_t :: FC.CULong
  }

deriving newtype instance F.Storable Uint64_t

newtype Uint32_t = Uint32_t
  { unUint32_t :: FC.CUInt
  }

deriving newtype instance F.Storable Uint32_t

data Foo = Foo
  { foo_sixty_four :: Uint64_t
  , foo_thirty_two :: Uint32_t
  }

instance F.Storable Foo where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               F.pokeByteOff ptr0 0 foo_sixty_four2
            >> F.pokeByteOff ptr0 8 foo_thirty_two3

deriving stock instance Show Foo

deriving stock instance Eq Foo
