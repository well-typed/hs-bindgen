{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), Eq, IO, Int, Ord, Show, pure)

newtype An_pchar = An_pchar
  { un_An_pchar :: F.Ptr FC.CChar
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

data MyCoolStruct = MyCoolStruct
  { myCoolStruct_listOfNames :: (HsBindgen.Runtime.ConstantArray.ConstantArray 8) ((HsBindgen.Runtime.ConstantArray.ConstantArray 255) FC.CChar)
  }
  deriving stock (Eq, Show)

instance F.Storable MyCoolStruct where

  sizeOf = \_ -> (2040 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure MyCoolStruct
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyCoolStruct myCoolStruct_listOfNames2 ->
            F.pokeByteOff ptr0 (0 :: Int) myCoolStruct_listOfNames2

newtype Foo = Foo
  { un_Foo :: F.FunPtr (FC.CInt -> IO FC.CInt)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
