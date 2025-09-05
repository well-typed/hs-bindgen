{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, Int, Show, pure, return)

{-| __C declaration:__ @foo@

    __defined at:__ @opaque_declaration.h:1:8@

    __exported by:__ @opaque_declaration.h@
-}
data Foo

{-| __C declaration:__ @bar@

    __defined at:__ @opaque_declaration.h:4:8@

    __exported by:__ @opaque_declaration.h@
-}
data Bar = Bar
  { bar_ptrA :: F.Ptr Foo
    {- ^ __C declaration:__ @ptrA@

         __defined at:__ @opaque_declaration.h:5:17@

         __exported by:__ @opaque_declaration.h@
    -}
  , bar_ptrB :: F.Ptr Bar
    {- ^ __C declaration:__ @ptrB@

         __defined at:__ @opaque_declaration.h:6:17@

         __exported by:__ @opaque_declaration.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_ptrA2
            >> F.pokeByteOff ptr0 (8 :: Int) bar_ptrB3

{-| __C declaration:__ @baz@

    __defined at:__ @opaque_declaration.h:9:8@

    __exported by:__ @opaque_declaration.h@
-}
data Baz = Baz
  {}
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure Baz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

{-| __C declaration:__ @quu@

    __defined at:__ @opaque_declaration.h:11:6@

    __exported by:__ @opaque_declaration.h@
-}
data Quu

{-| __C declaration:__ @opaque_union@

    __defined at:__ @opaque_declaration.h:13:7@

    __exported by:__ @opaque_declaration.h@
-}
data Opaque_union
