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
{-# LANGUAGE NoFieldSelectors #-}
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
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @uint@

    __defined at:__ @comprehensive\/smoke.h 7:22@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Uint = Uint
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" Uint ty where

  hasField =
    \x0 ->
      (\y1 -> Uint {unwrap = y1}, BG.getField @"unwrap" x0)

instance (ty ~ BG.CUInt) => BG.HasField "unwrap" (BG.Ptr Uint) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Uint "unwrap" where

  type CFieldType Uint "unwrap" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @size_t@

    __defined at:__ @comprehensive\/smoke.h 8:23@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Size_t = Size_t
  { unwrap :: BG.CULong
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance (ty ~ BG.CULong) => BG.CompatHasField.HasField "unwrap" Size_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Size_t {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CULong
         ) => BG.HasField "unwrap" (BG.Ptr Size_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Size_t "unwrap" where

  type CFieldType Size_t "unwrap" = BG.CULong

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar1_t@

    __defined at:__ @comprehensive\/smoke.h 39:8@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar1_t = Bar1_t
  { a :: BG.Ptr BG.Void
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 40:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , b :: BG.CInt
    {- ^ __C declaration:__ @b@

         __defined at:__ @comprehensive\/smoke.h 41:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , c :: BG.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @comprehensive\/smoke.h 42:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , d :: BG.Ptr BG.CChar
    {- ^ __C declaration:__ @d@

         __defined at:__ @comprehensive\/smoke.h 43:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , e :: BG.FunPtr (IO (BG.Ptr BG.CChar))
    {- ^ __C declaration:__ @e@

         __defined at:__ @comprehensive\/smoke.h 44:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , f :: BG.FunPtr (BG.Ptr BG.Void -> IO ())
    {- ^ __C declaration:__ @f@

         __defined at:__ @comprehensive\/smoke.h 45:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , g :: BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.CInt))
    {- ^ __C declaration:__ @g@

         __defined at:__ @comprehensive\/smoke.h 46:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , h :: BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr BG.CInt)))
    {- ^ __C declaration:__ @h@

         __defined at:__ @comprehensive\/smoke.h 47:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , i :: BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
    {- ^ __C declaration:__ @i@

         __defined at:__ @comprehensive\/smoke.h 48:13@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , j :: CA.ConstantArray 2 BG.CChar
    {- ^ __C declaration:__ @j@

         __defined at:__ @comprehensive\/smoke.h 49:11@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  , k :: BG.Ptr Bar1_t
    {- ^ __C declaration:__ @k@

         __defined at:__ @comprehensive\/smoke.h 51:19@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar1_t where

  staticSizeOf = \_ -> (80 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Bar1_t where

  readRaw =
    \ptr0 ->
          pure Bar1_t
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b") ptr0
      <*> HasCField.readRaw (BG.Proxy @"c") ptr0
      <*> HasCField.readRaw (BG.Proxy @"d") ptr0
      <*> HasCField.readRaw (BG.Proxy @"e") ptr0
      <*> HasCField.readRaw (BG.Proxy @"f") ptr0
      <*> HasCField.readRaw (BG.Proxy @"g") ptr0
      <*> HasCField.readRaw (BG.Proxy @"h") ptr0
      <*> HasCField.readRaw (BG.Proxy @"i") ptr0
      <*> HasCField.readRaw (BG.Proxy @"j") ptr0
      <*> HasCField.readRaw (BG.Proxy @"k") ptr0

instance Marshal.WriteRaw Bar1_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar1_t a2 b3 c4 d5 e6 f7 g8 h9 i10 j11 k12 ->
               HasCField.writeRaw (BG.Proxy @"a") ptr0 a2
            >> HasCField.writeRaw (BG.Proxy @"b") ptr0 b3
            >> HasCField.writeRaw (BG.Proxy @"c") ptr0 c4
            >> HasCField.writeRaw (BG.Proxy @"d") ptr0 d5
            >> HasCField.writeRaw (BG.Proxy @"e") ptr0 e6
            >> HasCField.writeRaw (BG.Proxy @"f") ptr0 f7
            >> HasCField.writeRaw (BG.Proxy @"g") ptr0 g8
            >> HasCField.writeRaw (BG.Proxy @"h") ptr0 h9
            >> HasCField.writeRaw (BG.Proxy @"i") ptr0 i10
            >> HasCField.writeRaw (BG.Proxy @"j") ptr0 j11
            >> HasCField.writeRaw (BG.Proxy @"k") ptr0 k12

deriving via Marshal.EquivStorable Bar1_t instance BG.Storable Bar1_t

{-| __C declaration:__ @a@

    __defined at:__ @comprehensive\/smoke.h 40:11@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.Ptr BG.Void) => BG.CompatHasField.HasField "a" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { a = y1
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"a" x0
      )

instance ( ty ~ BG.Ptr BG.Void
         ) => BG.HasField "a" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField Bar1_t "a" where

  type CFieldType Bar1_t "a" = BG.Ptr BG.Void

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @b@

    __defined at:__ @comprehensive\/smoke.h 41:11@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "b" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { b = y1
                 , a = BG.getField @"a" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"b" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "b" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b")

instance HasCField.HasCField Bar1_t "b" where

  type CFieldType Bar1_t "b" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @c@

    __defined at:__ @comprehensive\/smoke.h 42:11@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.CChar) => BG.CompatHasField.HasField "c" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { c = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"c" x0
      )

instance (ty ~ BG.CChar) => BG.HasField "c" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"c")

instance HasCField.HasCField Bar1_t "c" where

  type CFieldType Bar1_t "c" = BG.CChar

  offset# = \_ -> \_ -> 12

{-| __C declaration:__ @d@

    __defined at:__ @comprehensive\/smoke.h 43:11@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.CompatHasField.HasField "d" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { d = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"d" x0
      )

instance ( ty ~ BG.Ptr BG.CChar
         ) => BG.HasField "d" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"d")

instance HasCField.HasCField Bar1_t "d" where

  type CFieldType Bar1_t "d" = BG.Ptr BG.CChar

  offset# = \_ -> \_ -> 16

{-| __C declaration:__ @e@

    __defined at:__ @comprehensive\/smoke.h 44:13@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.FunPtr (IO (BG.Ptr BG.CChar))
         ) => BG.CompatHasField.HasField "e" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { e = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"e" x0
      )

instance ( ty ~ BG.FunPtr (IO (BG.Ptr BG.CChar))
         ) => BG.HasField "e" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"e")

instance HasCField.HasCField Bar1_t "e" where

  type CFieldType Bar1_t "e" =
    BG.FunPtr (IO (BG.Ptr BG.CChar))

  offset# = \_ -> \_ -> 24

{-| __C declaration:__ @f@

    __defined at:__ @comprehensive\/smoke.h 45:13@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO ())
         ) => BG.CompatHasField.HasField "f" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { f = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"f" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO ())
         ) => BG.HasField "f" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"f")

instance HasCField.HasCField Bar1_t "f" where

  type CFieldType Bar1_t "f" =
    BG.FunPtr (BG.Ptr BG.Void -> IO ())

  offset# = \_ -> \_ -> 32

{-| __C declaration:__ @g@

    __defined at:__ @comprehensive\/smoke.h 46:13@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.CInt))
         ) => BG.CompatHasField.HasField "g" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { g = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"g" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.CInt))
         ) => BG.HasField "g" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"g")

instance HasCField.HasCField Bar1_t "g" where

  type CFieldType Bar1_t "g" =
    BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr BG.CInt))

  offset# = \_ -> \_ -> 40

{-| __C declaration:__ @h@

    __defined at:__ @comprehensive\/smoke.h 47:13@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr BG.CInt)))
         ) => BG.CompatHasField.HasField "h" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { h = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"h" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr BG.CInt)))
         ) => BG.HasField "h" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"h")

instance HasCField.HasCField Bar1_t "h" where

  type CFieldType Bar1_t "h" =
    BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr BG.CInt)))

  offset# = \_ -> \_ -> 48

{-| __C declaration:__ @i@

    __defined at:__ @comprehensive\/smoke.h 48:13@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
         ) => BG.CompatHasField.HasField "i" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { i = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , j = BG.getField @"j" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"i" x0
      )

instance ( ty ~ BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))
         ) => BG.HasField "i" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"i")

instance HasCField.HasCField Bar1_t "i" where

  type CFieldType Bar1_t "i" =
    BG.FunPtr (BG.Ptr BG.Void -> IO (BG.Ptr (BG.Ptr (BG.Ptr BG.CInt))))

  offset# = \_ -> \_ -> 56

{-| __C declaration:__ @j@

    __defined at:__ @comprehensive\/smoke.h 49:11@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance ( ty ~ CA.ConstantArray 2 BG.CChar
         ) => BG.CompatHasField.HasField "j" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { j = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , k = BG.getField @"k" x0
                 }
      , BG.getField @"j" x0
      )

instance ( ty ~ CA.ConstantArray 2 BG.CChar
         ) => BG.HasField "j" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"j")

instance HasCField.HasCField Bar1_t "j" where

  type CFieldType Bar1_t "j" =
    CA.ConstantArray 2 BG.CChar

  offset# = \_ -> \_ -> 64

{-| __C declaration:__ @k@

    __defined at:__ @comprehensive\/smoke.h 51:19@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.Ptr Bar1_t) => BG.CompatHasField.HasField "k" Bar1_t ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Bar1_t { k = y1
                 , a = BG.getField @"a" x0
                 , b = BG.getField @"b" x0
                 , c = BG.getField @"c" x0
                 , d = BG.getField @"d" x0
                 , e = BG.getField @"e" x0
                 , f = BG.getField @"f" x0
                 , g = BG.getField @"g" x0
                 , h = BG.getField @"h" x0
                 , i = BG.getField @"i" x0
                 , j = BG.getField @"j" x0
                 }
      , BG.getField @"k" x0
      )

instance ( ty ~ BG.Ptr Bar1_t
         ) => BG.HasField "k" (BG.Ptr Bar1_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"k")

instance HasCField.HasCField Bar1_t "k" where

  type CFieldType Bar1_t "k" = BG.Ptr Bar1_t

  offset# = \_ -> \_ -> 72

{-| __C declaration:__ @struct bar2_t@

    __defined at:__ @comprehensive\/smoke.h 54:16@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar2_t = Bar2_t
  { a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 55:7@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar2_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar2_t where

  readRaw =
    \ptr0 ->
          pure Bar2_t
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0

instance Marshal.WriteRaw Bar2_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar2_t a2 ->
            HasCField.writeRaw (BG.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable Bar2_t instance BG.Storable Bar2_t

{-| __C declaration:__ @a@

    __defined at:__ @comprehensive\/smoke.h 55:7@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a" Bar2_t ty where

  hasField =
    \x0 -> (\y1 -> Bar2_t {a = y1}, BG.getField @"a" x0)

instance (ty ~ BG.CInt) => BG.HasField "a" (BG.Ptr Bar2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField Bar2_t "a" where

  type CFieldType Bar2_t "a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct bar3_t@

    __defined at:__ @comprehensive\/smoke.h 58:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
data Bar3_t = Bar3_t
  { a :: BG.CInt
    {- ^ __C declaration:__ @a@

         __defined at:__ @comprehensive\/smoke.h 59:7@

         __exported by:__ @comprehensive\/smoke.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Bar3_t where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Bar3_t where

  readRaw =
    \ptr0 ->
          pure Bar3_t
      <*> HasCField.readRaw (BG.Proxy @"a") ptr0

instance Marshal.WriteRaw Bar3_t where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar3_t a2 ->
            HasCField.writeRaw (BG.Proxy @"a") ptr0 a2

deriving via Marshal.EquivStorable Bar3_t instance BG.Storable Bar3_t

{-| __C declaration:__ @a@

    __defined at:__ @comprehensive\/smoke.h 59:7@

    __exported by:__ @comprehensive\/smoke.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "a" Bar3_t ty where

  hasField =
    \x0 -> (\y1 -> Bar3_t {a = y1}, BG.getField @"a" x0)

instance (ty ~ BG.CInt) => BG.HasField "a" (BG.Ptr Bar3_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"a")

instance HasCField.HasCField Bar3_t "a" where

  type CFieldType Bar3_t "a" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAZ1@

    __defined at:__ @comprehensive\/smoke.h 62:1@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ1 :: BG.CUInt
pattern BAZ1 = 1

{-| __C declaration:__ @enum baz2_t@

    __defined at:__ @comprehensive\/smoke.h 66:9@

    __exported by:__ @comprehensive\/smoke.h@
-}
newtype Baz2_t = Baz2_t
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Baz2_t instance BG.Storable Baz2_t

deriving via BG.CUInt instance BG.Prim Baz2_t

instance CEnum.CEnum Baz2_t where

  type CEnumZ Baz2_t = BG.CUInt

  toCEnum = Baz2_t

  fromCEnum = BG.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "BAZ2")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" Baz2_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Baz2_t {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrap" (BG.Ptr Baz2_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Baz2_t "unwrap" where

  type CFieldType Baz2_t "unwrap" = BG.CUInt

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
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Baz3_t instance BG.Storable Baz3_t

deriving via BG.CUInt instance BG.Prim Baz3_t

instance CEnum.CEnum Baz3_t where

  type CEnumZ Baz3_t = BG.CUInt

  toCEnum = Baz3_t

  fromCEnum = BG.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "BAZ3")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" Baz3_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Baz3_t {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrap" (BG.Ptr Baz3_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Baz3_t "unwrap" where

  type CFieldType Baz3_t "unwrap" = BG.CUInt

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
  { unwrap :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

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

deriving via Marshal.EquivStorable Baz4_t instance BG.Storable Baz4_t

deriving via BG.CUInt instance BG.Prim Baz4_t

instance CEnum.CEnum Baz4_t where

  type CEnumZ Baz4_t = BG.CUInt

  toCEnum = Baz4_t

  fromCEnum = BG.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(1, BG.singleton "BAZ4")]

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

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance (ty ~ BG.CUInt) => BG.CompatHasField.HasField "unwrap" Baz4_t ty where

  hasField =
    \x0 ->
      (\y1 ->
         Baz4_t {unwrap = y1}, BG.getField @"unwrap" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrap" (BG.Ptr Baz4_t) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"unwrap")

instance HasCField.HasCField Baz4_t "unwrap" where

  type CFieldType Baz4_t "unwrap" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @BAZ4@

    __defined at:__ @comprehensive\/smoke.h 75:3@

    __exported by:__ @comprehensive\/smoke.h@
-}
pattern BAZ4 :: Baz4_t
pattern BAZ4 = Baz4_t 1
