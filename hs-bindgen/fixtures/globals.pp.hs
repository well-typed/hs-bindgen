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

foreign import capi safe "&nesBool" nesBool :: F.Ptr FC.CBool

foreign import capi safe "&streamBinary" streamBinary :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)

foreign import capi safe "&streamBinary_len" streamBinary_len :: F.Ptr HsBindgen.Runtime.Prelude.Word32

data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.Prelude.Word8
  , version_t_minor :: HsBindgen.Runtime.Prelude.Word16
  , version_t_patch :: HsBindgen.Runtime.Prelude.Word8
  }
  deriving stock (Eq, Show)

instance F.Storable Version_t where

  sizeOf = \_ -> (6 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Version_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Version_t version_t_major2 version_t_minor3 version_t_patch4 ->
               F.pokeByteOff ptr0 (0 :: Int) version_t_major2
            >> F.pokeByteOff ptr0 (2 :: Int) version_t_minor3
            >> F.pokeByteOff ptr0 (4 :: Int) version_t_patch4

data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.Prelude.Word16
  , struct1_t_y :: FC.CBool
  , struct1_t_version :: Version_t
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1_t where

  sizeOf = \_ -> (10 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Struct1_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t struct1_t_x2 struct1_t_y3 struct1_t_version4 ->
               F.pokeByteOff ptr0 (0 :: Int) struct1_t_x2
            >> F.pokeByteOff ptr0 (2 :: Int) struct1_t_y3
            >> F.pokeByteOff ptr0 (4 :: Int) struct1_t_version4

data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2_t where

  sizeOf = \_ -> (10 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Struct2_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t struct2_t_field12 -> F.pokeByteOff ptr0 (0 :: Int) struct2_t_field12

foreign import capi safe "&some_global_struct" some_global_struct :: F.Ptr Struct2_t
