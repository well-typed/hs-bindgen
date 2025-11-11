{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @fixedwidth.h:3:8@

    __exported by:__ @fixedwidth.h@
-}
data Foo = Foo
  { foo_sixty_four :: HsBindgen.Runtime.Prelude.Word64
    {- ^ __C declaration:__ @sixty_four@

         __defined at:__ @fixedwidth.h:4:11@

         __exported by:__ @fixedwidth.h@
    -}
  , foo_thirty_two :: HsBindgen.Runtime.Prelude.Word32
    {- ^ __C declaration:__ @thirty_two@

         __defined at:__ @fixedwidth.h:5:11@

         __exported by:__ @fixedwidth.h@
    -}
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
