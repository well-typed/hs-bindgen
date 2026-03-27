{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Uint(..)
    , Example.Size_t(..)
    , Example.Bar1_t(..)
    , Example.Bar2_t(..)
    , Example.Bar3_t(..)
    , pattern Example.BAZ1
    , Example.Baz2_t(..)
    , pattern Example.BAZ2
    , Example.Baz3_t(..)
    , pattern Example.BAZ3
    , Example.Baz4_t(..)
    , pattern Example.BAZ4
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @uint@

    __defined at:__ @comprehensive\/smoke.h 7:22@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Uint = Uint
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Uint) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Uint "unwrap" where

  type CFieldType Uint "unwrap" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @size_t@

    __defined at:__ @comprehensive\/smoke.h 8:23@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Size_t = Size_t
  { unwrap :: RIP.CULong
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CULong
         ) => RIP.HasField "unwrap" (RIP.Ptr Size_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Size_t "unwrap" where

  type CFieldType Size_t "unwrap" = RIP.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar1_t@

    __defined at:__ @comprehensive\/smoke.h 39:8@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar1_t = Bar1_t
  { a :: RIP.Ptr RIP.Void
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 40:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , b :: RIP.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @comprehensive\/smoke.h 41:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , c :: RIP.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @comprehensive\/smoke.h 42:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , d :: RIP.Ptr RIP.CChar
    {- ^ __C declaration:__ @d@

         __defined at:__ @comprehensive\/smoke.h 43:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , e :: RIP.FunPtr (IO (RIP.Ptr RIP.CChar))
    {- ^ __C declaration:__ @e@

         __defined at:__ @comprehensive\/smoke.h 44:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , f :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())
    {- ^ __C declaration:__ @f@

         __defined at:__ @comprehensive\/smoke.h 45:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , g :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr RIP.CInt))
    {- ^ __C declaration:__ @g@

         __defined at:__ @comprehensive\/smoke.h 46:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , h :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))
    {- ^ __C declaration:__ @h@

         __defined at:__ @comprehensive\/smoke.h 47:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , i :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))
    {- ^ __C declaration:__ @i@

         __defined at:__ @comprehensive\/smoke.h 48:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , j :: (CA.ConstantArray 2) RIP.CChar
    {- ^ __C declaration:__ @j@

         __defined at:__ @comprehensive\/smoke.h 49:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , k :: RIP.Ptr Bar1_t
    {- ^ __C declaration:__ @k@

         __defined at:__ @comprehensive\/smoke.h 51:19@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar1_t where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bar1_t where

  readRaw =
    \ptr0 ->
          pure Bar1_t
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"b") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"c") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"d") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"e") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"f") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"g") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"h") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"i") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"j") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"k") ptr0

instance Marshal.WriteRaw Bar1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar1_t a2 b3 c4 d5 e6 f7 g8 h9 i10 j11 k12 ->
               HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (RIP.Proxy @"b") ptr0 b3
            >> HasCField.writeRaw (RIP.Proxy @"c") ptr0 c4
            >> HasCField.writeRaw (RIP.Proxy @"d") ptr0 d5
            >> HasCField.writeRaw (RIP.Proxy @"e") ptr0 e6
            >> HasCField.writeRaw (RIP.Proxy @"f") ptr0 f7
            >> HasCField.writeRaw (RIP.Proxy @"g") ptr0 g8
            >> HasCField.writeRaw (RIP.Proxy @"h") ptr0 h9
            >> HasCField.writeRaw (RIP.Proxy @"i") ptr0 i10
            >> HasCField.writeRaw (RIP.Proxy @"j") ptr0 j11
            >> HasCField.writeRaw (RIP.Proxy @"k") ptr0 k12

deriving via Marshal.EquivStorable Bar1_t instance RIP.Storable Bar1_t

instance HasCField.HasCField Bar1_t "a" where

  type CFieldType Bar1_t "a" = RIP.Ptr RIP.Void

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr RIP.Void)
         ) => RIP.HasField "a" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField Bar1_t "b" where

  type CFieldType Bar1_t "b" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "b" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

instance HasCField.HasCField Bar1_t "c" where

  type CFieldType Bar1_t "c" = RIP.CChar

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "c" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"c")

instance HasCField.HasCField Bar1_t "d" where

  type CFieldType Bar1_t "d" = RIP.Ptr RIP.CChar

  offset# = \_ -> \_ -> 16

instance ( ((~) ty) (RIP.Ptr RIP.CChar)
         ) => RIP.HasField "d" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"d")

instance HasCField.HasCField Bar1_t "e" where

  type CFieldType Bar1_t "e" =
    RIP.FunPtr (IO (RIP.Ptr RIP.CChar))

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) (RIP.FunPtr (IO (RIP.Ptr RIP.CChar)))
         ) => RIP.HasField "e" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"e")

instance HasCField.HasCField Bar1_t "f" where

  type CFieldType Bar1_t "f" =
    RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ())

  offset# = \_ -> \_ -> 32

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO ()))
         ) => RIP.HasField "f" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"f")

instance HasCField.HasCField Bar1_t "g" where

  type CFieldType Bar1_t "g" =
    RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr RIP.CInt))

  offset# = \_ -> \_ -> 40

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr RIP.CInt)))
         ) => RIP.HasField "g" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"g")

instance HasCField.HasCField Bar1_t "h" where

  type CFieldType Bar1_t "h" =
    RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr RIP.CInt)))

  offset# = \_ -> \_ -> 48

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr RIP.CInt))))
         ) => RIP.HasField "h" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"h")

instance HasCField.HasCField Bar1_t "i" where

  type CFieldType Bar1_t "i" =
    RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt))))

  offset# = \_ -> \_ -> 56

instance ( ((~) ty) (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO (RIP.Ptr (RIP.Ptr (RIP.Ptr RIP.CInt)))))
         ) => RIP.HasField "i" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"i")

instance HasCField.HasCField Bar1_t "j" where

  type CFieldType Bar1_t "j" =
    (CA.ConstantArray 2) RIP.CChar

  offset# = \_ -> \_ -> 64

instance ( ((~) ty) ((CA.ConstantArray 2) RIP.CChar)
         ) => RIP.HasField "j" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"j")

instance HasCField.HasCField Bar1_t "k" where

  type CFieldType Bar1_t "k" = RIP.Ptr Bar1_t

  offset# = \_ -> \_ -> 72

instance ( ((~) ty) (RIP.Ptr Bar1_t)
         ) => RIP.HasField "k" (RIP.Ptr Bar1_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"k")

{-| __C declaration:__ @struct bar2_t@

    __defined at:__ @comprehensive\/smoke.h 54:16@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar2_t = Bar2_t
  { a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 55:7@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar2_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar2_t where

  readRaw =
    \ptr0 ->
          pure Bar2_t
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0

instance Marshal.WriteRaw Bar2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar2_t a2 ->
            HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable Bar2_t instance RIP.Storable Bar2_t

instance HasCField.HasCField Bar2_t "a" where

  type CFieldType Bar2_t "a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "a" (RIP.Ptr Bar2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

{-| __C declaration:__ @struct bar3_t@

    __defined at:__ @comprehensive\/smoke.h 58:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar3_t = Bar3_t
  { a :: RIP.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 59:7@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Bar3_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar3_t where

  readRaw =
    \ptr0 ->
          pure Bar3_t
      <*> HasCField.readRaw (RIP.Proxy @"a") ptr0

instance Marshal.WriteRaw Bar3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar3_t a2 ->
            HasCField.writeRaw (RIP.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable Bar3_t instance RIP.Storable Bar3_t

instance HasCField.HasCField Bar3_t "a" where

  type CFieldType Bar3_t "a" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "a" (RIP.Ptr Bar3_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

{-| __C declaration:__ @BAZ1@

    __defined at:__ @comprehensive\/smoke.h 62:1@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ1 :: RIP.CUInt
pattern BAZ1 = 1

{-| __C declaration:__ @enum baz2_t@

    __defined at:__ @comprehensive\/smoke.h 66:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Baz2_t = Baz2_t
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Baz2_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Baz2_t where

  readRaw =
    \ptr0 ->
          pure Baz2_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Baz2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz2_t unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable Baz2_t instance RIP.Storable Baz2_t

deriving via RIP.CUInt instance RIP.Prim Baz2_t

instance CEnum.CEnum Baz2_t where

  type CEnumZ Baz2_t = RIP.CUInt

  toCEnum = Baz2_t

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, RIP.singleton "BAZ2")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Baz2_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Baz2_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Baz2_t where

  minDeclaredValue = BAZ2

  maxDeclaredValue = BAZ2

instance Show Baz2_t where

  showsPrec = CEnum.shows

instance Read Baz2_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Baz2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Baz2_t "unwrap" where

  type CFieldType Baz2_t "unwrap" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAZ2@

    __defined at:__ @comprehensive\/smoke.h 67:3@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ2 :: Baz2_t
pattern BAZ2 = Baz2_t 1

{-| __C declaration:__ @enum baz3_t@

    __defined at:__ @comprehensive\/smoke.h 70:6@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Baz3_t = Baz3_t
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Baz3_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Baz3_t where

  readRaw =
    \ptr0 ->
          pure Baz3_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Baz3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz3_t unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable Baz3_t instance RIP.Storable Baz3_t

deriving via RIP.CUInt instance RIP.Prim Baz3_t

instance CEnum.CEnum Baz3_t where

  type CEnumZ Baz3_t = RIP.CUInt

  toCEnum = Baz3_t

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, RIP.singleton "BAZ3")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Baz3_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Baz3_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Baz3_t where

  minDeclaredValue = BAZ3

  maxDeclaredValue = BAZ3

instance Show Baz3_t where

  showsPrec = CEnum.shows

instance Read Baz3_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Baz3_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Baz3_t "unwrap" where

  type CFieldType Baz3_t "unwrap" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAZ3@

    __defined at:__ @comprehensive\/smoke.h 71:3@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ3 :: Baz3_t
pattern BAZ3 = Baz3_t 1

{-| __C declaration:__ @enum baz4_t@

    __defined at:__ @comprehensive\/smoke.h 74:14@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Baz4_t = Baz4_t
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize Baz4_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Baz4_t where

  readRaw =
    \ptr0 ->
          pure Baz4_t
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Baz4_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz4_t unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable Baz4_t instance RIP.Storable Baz4_t

deriving via RIP.CUInt instance RIP.Prim Baz4_t

instance CEnum.CEnum Baz4_t where

  type CEnumZ Baz4_t = RIP.CUInt

  toCEnum = Baz4_t

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, RIP.singleton "BAZ4")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Baz4_t"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Baz4_t"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Baz4_t where

  minDeclaredValue = BAZ4

  maxDeclaredValue = BAZ4

instance Show Baz4_t where

  showsPrec = CEnum.shows

instance Read Baz4_t where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Baz4_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Baz4_t "unwrap" where

  type CFieldType Baz4_t "unwrap" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAZ4@

    __defined at:__ @comprehensive\/smoke.h 75:3@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ4 :: Baz4_t
pattern BAZ4 = Baz4_t 1
