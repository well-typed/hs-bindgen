{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as F
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

{-| __C declaration:__ @foo@

    __defined at:__ @attributes.h:10:36@

    __exported by:__ @attributes.h@
-}
data Foo = Foo
  { foo_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes.h:11:10@

         __exported by:__ @attributes.h@
    -}
  , foo_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes.h:12:10@

         __exported by:__ @attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Foo where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_c2 foo_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) foo_c2
            >> F.pokeByteOff ptr0 (1 :: Int) foo_i3

{-| __C declaration:__ @bar@

    __defined at:__ @attributes.h:16:15@

    __exported by:__ @attributes.h@
-}
data Bar = Bar
  { bar_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes.h:17:10@

         __exported by:__ @attributes.h@
    -}
  , bar_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes.h:18:10@

         __exported by:__ @attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Bar where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_c2 bar_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) bar_c2
            >> F.pokeByteOff ptr0 (1 :: Int) bar_i3

{-| __C declaration:__ @baz@

    __defined at:__ @attributes.h:22:9@

    __exported by:__ @attributes.h@
-}
data Baz = Baz
  { baz_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes.h:23:10@

         __exported by:__ @attributes.h@
    -}
  , baz_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes.h:24:10@

         __exported by:__ @attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Baz where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Baz
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz baz_c2 baz_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) baz_c2
            >> F.pokeByteOff ptr0 (1 :: Int) baz_i3

{-| __C declaration:__ @qux@

    __defined at:__ @attributes.h:28:9@

    __exported by:__ @attributes.h@
-}
data Qux = Qux
  { qux_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @attributes.h:29:10@

         __exported by:__ @attributes.h@
    -}
  , qux_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @attributes.h:30:10@

         __exported by:__ @attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Qux where

  sizeOf = \_ -> (5 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Qux
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Qux qux_c2 qux_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) qux_c2
            >> F.pokeByteOff ptr0 (1 :: Int) qux_i3

{-| __C declaration:__ @FILE@

    __defined at:__ @attributes.h:34:16@

    __exported by:__ @attributes.h@
-}
data FILE = FILE
  { fILE__r :: FC.CInt
    {- ^ __C declaration:__ @_r@

         __defined at:__ @attributes.h:35:9@

         __exported by:__ @attributes.h@
    -}
  , fILE__w :: FC.CInt
    {- ^ __C declaration:__ @_w@

         __defined at:__ @attributes.h:36:9@

         __exported by:__ @attributes.h@
    -}
  , fILE__close :: F.FunPtr ((F.Ptr Void) -> IO FC.CInt)
    {- ^ __C declaration:__ @_close@

         __defined at:__ @attributes.h:37:22@

         __exported by:__ @attributes.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure FILE
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE fILE__r2 fILE__w3 fILE__close4 ->
               F.pokeByteOff ptr0 (0 :: Int) fILE__r2
            >> F.pokeByteOff ptr0 (4 :: Int) fILE__w3
            >> F.pokeByteOff ptr0 (8 :: Int) fILE__close4
