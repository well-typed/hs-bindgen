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
  { c__SFILE__r :: FC.CInt
  , c__SFILE__w :: FC.CInt
  , c__SFILE__close :: F.FunPtr ((F.Ptr Void) -> IO FC.CInt)
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
          C__SFILE c__SFILE__r2 c__SFILE__w3 c__SFILE__close4 ->
               F.pokeByteOff ptr0 (0 :: Int) c__SFILE__r2
            >> F.pokeByteOff ptr0 (4 :: Int) c__SFILE__w3
            >> F.pokeByteOff ptr0 (8 :: Int) c__SFILE__close4

deriving stock instance Show C__SFILE

deriving stock instance Eq C__SFILE

newtype FILE = FILE
  { un_FILE :: C__SFILE
  }

deriving newtype instance F.Storable FILE

deriving stock instance Eq FILE

deriving stock instance Show FILE
