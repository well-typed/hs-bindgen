{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Linked_list_A_s = Linked_list_A_s
  { linked_list_A_s_x :: FC.CInt
  , linked_list_A_s_next :: F.Ptr Linked_list_A_s
  }
  deriving stock (Eq, Show)

instance F.Storable Linked_list_A_s where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Linked_list_A_s
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_A_s linked_list_A_s_x2 linked_list_A_s_next3 ->
               F.pokeByteOff ptr0 (0 :: Int) linked_list_A_s_x2
            >> F.pokeByteOff ptr0 (8 :: Int) linked_list_A_s_next3

newtype Linked_list_A_t = Linked_list_A_t
  { un_Linked_list_A_t :: Linked_list_A_s
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data Linked_list_B_t = Linked_list_B_t
  { linked_list_B_t_x :: FC.CInt
  , linked_list_B_t_next :: F.Ptr Linked_list_B_t
  }
  deriving stock (Eq, Show)

instance F.Storable Linked_list_B_t where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Linked_list_B_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Linked_list_B_t linked_list_B_t_x2 linked_list_B_t_next3 ->
               F.pokeByteOff ptr0 (0 :: Int) linked_list_B_t_x2
            >> F.pokeByteOff ptr0 (8 :: Int) linked_list_B_t_next3
