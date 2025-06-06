{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

data C__SFILE = C__SFILE
  { __sFILE__r :: FC.CInt
  , __sFILE__w :: FC.CInt
  , __sFILE__close :: F.FunPtr ((F.Ptr Void) -> IO FC.CInt)
  }

instance F.Storable C__SFILE where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure C__SFILE
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          C__SFILE __sFILE__r2 __sFILE__w3 __sFILE__close4 ->
               F.pokeByteOff ptr0 (0 :: Int) __sFILE__r2
            >> F.pokeByteOff ptr0 (4 :: Int) __sFILE__w3
            >> F.pokeByteOff ptr0 (8 :: Int) __sFILE__close4

deriving stock instance Show C__SFILE

deriving stock instance Eq C__SFILE

newtype FILE = FILE
  { un_FILE :: C__SFILE
  }

deriving newtype instance F.Storable FILE

deriving stock instance Eq FILE

deriving stock instance Show FILE
