{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

$(CAPI.addCSource "#include \"globals.h\"\n")

foreign import capi safe "&simpleGlobal" simpleGlobal :: F.Ptr FC.CInt

data Config = Config
  { config_x :: FC.CInt
  , config_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Config where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Config
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config config_x2 config_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) config_x2
            >> F.pokeByteOff ptr0 (4 :: Int) config_y3

foreign import capi safe "&compoundGlobal1" compoundGlobal1 :: F.Ptr Config

data Inline_struct = Inline_struct
  { inline_struct_x :: FC.CInt
  , inline_struct_y :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable Inline_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Inline_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inline_struct inline_struct_x2 inline_struct_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) inline_struct_x2
            >> F.pokeByteOff ptr0 (4 :: Int) inline_struct_y3

foreign import capi safe "&compoundGlobal2" compoundGlobal2 :: F.Ptr Inline_struct

foreign import capi safe "&nesInteger" nesInteger :: F.Ptr FC.CInt

foreign import capi safe "&nesFloating" nesFloating :: F.Ptr FC.CFloat

foreign import capi safe "&nesString1" nesString1 :: F.Ptr (F.Ptr FC.CChar)

foreign import capi safe "&nesString2" nesString2 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)

foreign import capi safe "&nesCharacter" nesCharacter :: F.Ptr FC.CChar

foreign import capi safe "&nesParen" nesParen :: F.Ptr FC.CInt

foreign import capi safe "&nesUnary" nesUnary :: F.Ptr FC.CInt

foreign import capi safe "&nesBinary" nesBinary :: F.Ptr FC.CInt

foreign import capi safe "&nesConditional" nesConditional :: F.Ptr FC.CInt

foreign import capi safe "&nesCast" nesCast :: F.Ptr FC.CFloat

foreign import capi safe "&nesCompound" nesCompound :: F.Ptr (F.Ptr FC.CInt)

foreign import capi safe "&nesInitList" nesInitList :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
