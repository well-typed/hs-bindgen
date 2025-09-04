{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

{-| __C declaration:__ @S1_t@

    __defined at:__ @forward_declaration.h:3:8@

    __exported by:__ @forward_declaration.h@
-}
data S1_t = S1_t
  { s1_t_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @forward_declaration.h:4:7@

         __exported by:__ @forward_declaration.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S1_t where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1_t s1_t_a2 -> F.pokeByteOff ptr0 (0 :: Int) s1_t_a2

{-| __C declaration:__ @S2@

    __defined at:__ @forward_declaration.h:9:8@

    __exported by:__ @forward_declaration.h@
-}
data S2 = S2
  { s2_a :: FC.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @forward_declaration.h:10:7@

         __exported by:__ @forward_declaration.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 -> F.pokeByteOff ptr0 (0 :: Int) s2_a2
