{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Pascal_Aux(..)
    , Example.Pascal
    , Example.Foo_bar(..)
    , Example.Foo_Aux(..)
    , Example.Foo
    , Example.Diff_Aux(..)
    , Example.Diff
    , Example.Triplets_Aux(..)
    , Example.Triplets
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Pascal_Aux = Pascal
  { pascal_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 3:9@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Pascal_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Pascal_Aux where

  readRaw =
    \ptr0 ->
          pure Pascal
      <*> HasCField.readRaw (BG.Proxy @"pascal_len") ptr0

instance Marshal.WriteRaw Pascal_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 ->
            HasCField.writeRaw (BG.Proxy @"pascal_len") ptr0 pascal_len2

deriving via Marshal.EquivStorable Pascal_Aux instance BG.Storable Pascal_Aux

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "pascal_len" Pascal_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Pascal {pascal_len = y1}, BG.getField @"pascal_len" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "pascal_len" (BG.Ptr Pascal_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"pascal_len")

instance HasCField.HasCField Pascal_Aux "pascal_len" where

  type CFieldType Pascal_Aux "pascal_len" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset BG.CChar Pascal_Aux where

  offset = \_proxy0 -> 4

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Pascal = FLAM.WithFlam BG.CChar Pascal_Aux

{-| __C declaration:__ @struct \@foo_bar@

    __defined at:__ @edge-cases\/flam.h 10:2@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_bar = Foo_bar
  { foo_bar_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/flam.h 11:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , foo_bar_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/flam.h 12:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo_bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_bar where

  readRaw =
    \ptr0 ->
          pure Foo_bar
      <*> HasCField.readRaw (BG.Proxy @"foo_bar_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"foo_bar_y") ptr0

instance Marshal.WriteRaw Foo_bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               HasCField.writeRaw (BG.Proxy @"foo_bar_x") ptr0 foo_bar_x2
            >> HasCField.writeRaw (BG.Proxy @"foo_bar_y") ptr0 foo_bar_y3

deriving via Marshal.EquivStorable Foo_bar instance BG.Storable Foo_bar

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "foo_bar_x" Foo_bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo_bar {foo_bar_x = y1, foo_bar_y = BG.getField @"foo_bar_y" x0}
      , BG.getField @"foo_bar_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_bar_x" (BG.Ptr Foo_bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_bar_x")

instance HasCField.HasCField Foo_bar "foo_bar_x" where

  type CFieldType Foo_bar "foo_bar_x" = BG.CInt

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "foo_bar_y" Foo_bar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Foo_bar {foo_bar_y = y1, foo_bar_x = BG.getField @"foo_bar_x" x0}
      , BG.getField @"foo_bar_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_bar_y" (BG.Ptr Foo_bar) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_bar_y")

instance HasCField.HasCField Foo_bar "foo_bar_y" where

  type CFieldType Foo_bar "foo_bar_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_Aux = Foo
  { foo_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 9:6@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Foo_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_Aux where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (BG.Proxy @"foo_len") ptr0

instance Marshal.WriteRaw Foo_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 ->
            HasCField.writeRaw (BG.Proxy @"foo_len") ptr0 foo_len2

deriving via Marshal.EquivStorable Foo_Aux instance BG.Storable Foo_Aux

instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "foo_len" Foo_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Foo {foo_len = y1}, BG.getField @"foo_len" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "foo_len" (BG.Ptr Foo_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"foo_len")

instance HasCField.HasCField Foo_Aux "foo_len" where

  type CFieldType Foo_Aux "foo_len" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset Foo_bar Foo_Aux where

  offset = \_proxy0 -> 4

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Foo = FLAM.WithFlam Foo_bar Foo_Aux

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Diff_Aux = Diff
  { diff_first :: BG.CLong
    {- ^ __C declaration:__ @first@

         __defined at:__ @edge-cases\/flam.h 18:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , diff_second :: BG.CChar
    {- ^ __C declaration:__ @second@

         __defined at:__ @edge-cases\/flam.h 19:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Diff_Aux where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Diff_Aux where

  readRaw =
    \ptr0 ->
          pure Diff
      <*> HasCField.readRaw (BG.Proxy @"diff_first") ptr0
      <*> HasCField.readRaw (BG.Proxy @"diff_second") ptr0

instance Marshal.WriteRaw Diff_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               HasCField.writeRaw (BG.Proxy @"diff_first") ptr0 diff_first2
            >> HasCField.writeRaw (BG.Proxy @"diff_second") ptr0 diff_second3

deriving via Marshal.EquivStorable Diff_Aux instance BG.Storable Diff_Aux

instance ( ty ~ BG.CLong
         ) => BG.CompatHasField.HasField "diff_first" Diff_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Diff {diff_first = y1, diff_second = BG.getField @"diff_second" x0}
      , BG.getField @"diff_first" x0
      )

instance ( ty ~ BG.CLong
         ) => BG.HasField "diff_first" (BG.Ptr Diff_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"diff_first")

instance HasCField.HasCField Diff_Aux "diff_first" where

  type CFieldType Diff_Aux "diff_first" = BG.CLong

  offset# = \_ -> \_ -> 0

instance ( ty ~ BG.CChar
         ) => BG.CompatHasField.HasField "diff_second" Diff_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Diff {diff_second = y1, diff_first = BG.getField @"diff_first" x0}
      , BG.getField @"diff_second" x0
      )

instance ( ty ~ BG.CChar
         ) => BG.HasField "diff_second" (BG.Ptr Diff_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"diff_second")

instance HasCField.HasCField Diff_Aux "diff_second" where

  type CFieldType Diff_Aux "diff_second" = BG.CChar

  offset# = \_ -> \_ -> 8

instance FLAM.Offset BG.CChar Diff_Aux where

  offset = \_proxy0 -> 9

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Diff = FLAM.WithFlam BG.CChar Diff_Aux

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

    __C declaration:__ @struct triplets@

    __defined at:__ @edge-cases\/flam.h 26:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Triplets_Aux = Triplets
  { triplets_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 27:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Triplets_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Triplets_Aux where

  readRaw =
    \ptr0 ->
          pure Triplets
      <*> HasCField.readRaw (BG.Proxy @"triplets_len") ptr0

instance Marshal.WriteRaw Triplets_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triplets triplets_len2 ->
            HasCField.writeRaw (BG.Proxy @"triplets_len") ptr0 triplets_len2

deriving via Marshal.EquivStorable Triplets_Aux instance BG.Storable Triplets_Aux

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "triplets_len" Triplets_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Triplets {triplets_len = y1}, BG.getField @"triplets_len" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "triplets_len" (BG.Ptr Triplets_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"triplets_len")

instance HasCField.HasCField Triplets_Aux "triplets_len" where

  type CFieldType Triplets_Aux "triplets_len" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset (CA.ConstantArray 3 BG.CInt) Triplets_Aux where

  offset = \_proxy0 -> 4

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

    __C declaration:__ @struct triplets@

    __defined at:__ @edge-cases\/flam.h 26:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Triplets =
  FLAM.WithFlam (CA.ConstantArray 3 BG.CInt) Triplets_Aux
