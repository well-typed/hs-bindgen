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

{-| __C declaration:__ @pascal@

    __defined at:__ @flam.h:2:8@

    __exported by:__ @flam.h@
-}
data Pascal = Pascal
  { pascal_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @flam.h:3:9@

         __exported by:__ @flam.h@
    -}
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

{-| __C declaration:__ @foo_bar@

    __defined at:__ @flam.h:10:2@

    __exported by:__ @flam.h@
-}
data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @flam.h:11:7@

         __exported by:__ @flam.h@
    -}
  , foo_bar_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @flam.h:12:7@

         __exported by:__ @flam.h@
    -}
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

{-| __C declaration:__ @foo@

    __defined at:__ @flam.h:8:8@

    __exported by:__ @flam.h@
-}
data Foo = Foo
  { foo_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @flam.h:9:6@

         __exported by:__ @flam.h@
    -}
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

{-| __C declaration:__ @diff@

    __defined at:__ @flam.h:17:8@

    __exported by:__ @flam.h@
-}
data Diff = Diff
  { diff_first :: FC.CLong
    {- ^ __C declaration:__ @first@

         __defined at:__ @flam.h:18:7@

         __exported by:__ @flam.h@
    -}
  , diff_second :: FC.CChar
    {- ^ __C declaration:__ @second@

         __defined at:__ @flam.h:19:7@

         __exported by:__ @flam.h@
    -}
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

data Triplets = Triplets
  { triplets_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @flam.h:27:7@

         __exported by:__ @flam.h@
    -}
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
