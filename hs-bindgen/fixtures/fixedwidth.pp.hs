{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Foo = Foo
  { foo_sixty_four :: HsBindgen.Runtime.Prelude.Word64
  , foo_thirty_two :: HsBindgen.Runtime.Prelude.Word32
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_sixty_four2 foo_thirty_two3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_sixty_four2
            >> F.pokeByteOff ptr0 (8 :: Int) foo_thirty_two3
