{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Bits (FiniteBits)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @config@

    __defined at:__ @globals.h:12:8@

    __exported by:__ @globals.h@
-}
data Config = Config
  { config_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:13:7@

         __exported by:__ @globals.h@
    -}
  , config_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:14:7@

         __exported by:__ @globals.h@
    -}
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

{-| __C declaration:__ @inline_struct@

    __defined at:__ @globals.h:19:15@

    __exported by:__ @globals.h@
-}
data Inline_struct = Inline_struct
  { inline_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:19:35@

         __exported by:__ @globals.h@
    -}
  , inline_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:19:42@

         __exported by:__ @globals.h@
    -}
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

{-| __defined at:__ @globals.h:406:9@

    __exported by:__ @globals.h@
-}
data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.Prelude.Word8
    {- ^ __C declaration:__ @major@

         __defined at:__ @globals.h:408:12@

         __exported by:__ @globals.h@
    -}
  , version_t_minor :: HsBindgen.Runtime.Prelude.Word16
    {- ^ __C declaration:__ @minor@

         __defined at:__ @globals.h:409:12@

         __exported by:__ @globals.h@
    -}
  , version_t_patch :: HsBindgen.Runtime.Prelude.Word8
    {- ^ __C declaration:__ @patch@

         __defined at:__ @globals.h:410:12@

         __exported by:__ @globals.h@
    -}
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

{-| __defined at:__ @globals.h:413:9@

    __exported by:__ @globals.h@
-}
data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.Prelude.Word16
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:415:13@

         __exported by:__ @globals.h@
    -}
  , struct1_t_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:416:13@

         __exported by:__ @globals.h@
    -}
  , struct1_t_version :: Version_t
    {- ^ __C declaration:__ @version@

         __defined at:__ @globals.h:417:13@

         __exported by:__ @globals.h@
    -}
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

{-| __defined at:__ @globals.h:420:9@

    __exported by:__ @globals.h@
-}
data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
    {- ^ __C declaration:__ @field1@

         __defined at:__ @globals.h:422:13@

         __exported by:__ @globals.h@
    -}
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
          Struct2_t struct2_t_field12 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_t_field12

{-| Constant, through typedef

__C declaration:__ @ConstInt@

__defined at:__ @globals.h:448:19@

__exported by:__ @globals.h@
-}
newtype ConstInt = ConstInt
  { un_ConstInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| An array of uknown size containing constant integers

__C declaration:__ @ConstIntArray@

__defined at:__ @globals.h:463:19@

__exported by:__ @globals.h@
-}
newtype ConstIntArray = ConstIntArray
  { un_ConstIntArray :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @tuple@

    __defined at:__ @globals.h:466:8@

    __exported by:__ @globals.h@
-}
data Tuple = Tuple
  { tuple_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:466:20@

         __exported by:__ @globals.h@
    -}
  , tuple_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:466:33@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Tuple where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Tuple
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tuple tuple_x2 tuple_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) tuple_x2
            >> F.pokeByteOff ptr0 (4 :: Int) tuple_y3
