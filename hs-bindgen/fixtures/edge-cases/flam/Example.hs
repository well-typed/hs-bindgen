{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Pascal_Aux = Pascal
  { pascal_len :: RIP.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 3:9@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Pascal_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Pascal_Aux where

  readRaw =
    \ptr0 ->
          pure Pascal
      <*> HasCField.readRaw (RIP.Proxy @"pascal_len") ptr0

instance Marshal.WriteRaw Pascal_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 ->
            HasCField.writeRaw (RIP.Proxy @"pascal_len") ptr0 pascal_len2

deriving via Marshal.EquivStorable Pascal_Aux instance RIP.Storable Pascal_Aux

instance HasCField.HasCField Pascal_Aux "pascal_len" where

  type CFieldType Pascal_Aux "pascal_len" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "pascal_len" (RIP.Ptr Pascal_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"pascal_len")

instance FLAM.Offset RIP.CChar Pascal_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Pascal = (FLAM.WithFlam RIP.CChar) Pascal_Aux

{-| __C declaration:__ @struct \@foo_bar@

    __defined at:__ @edge-cases\/flam.h 10:2@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_bar = Foo_bar
  { foo_bar_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/flam.h 11:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , foo_bar_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/flam.h 12:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_bar where

  readRaw =
    \ptr0 ->
          pure Foo_bar
      <*> HasCField.readRaw (RIP.Proxy @"foo_bar_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"foo_bar_y") ptr0

instance Marshal.WriteRaw Foo_bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               HasCField.writeRaw (RIP.Proxy @"foo_bar_x") ptr0 foo_bar_x2
            >> HasCField.writeRaw (RIP.Proxy @"foo_bar_y") ptr0 foo_bar_y3

deriving via Marshal.EquivStorable Foo_bar instance RIP.Storable Foo_bar

instance HasCField.HasCField Foo_bar "foo_bar_x" where

  type CFieldType Foo_bar "foo_bar_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_bar_x" (RIP.Ptr Foo_bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_bar_x")

instance HasCField.HasCField Foo_bar "foo_bar_y" where

  type CFieldType Foo_bar "foo_bar_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_bar_y" (RIP.Ptr Foo_bar) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_bar_y")

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_Aux = Foo
  { foo_len :: RIP.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 9:6@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Foo_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Foo_Aux where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HasCField.readRaw (RIP.Proxy @"foo_len") ptr0

instance Marshal.WriteRaw Foo_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 ->
            HasCField.writeRaw (RIP.Proxy @"foo_len") ptr0 foo_len2

deriving via Marshal.EquivStorable Foo_Aux instance RIP.Storable Foo_Aux

instance HasCField.HasCField Foo_Aux "foo_len" where

  type CFieldType Foo_Aux "foo_len" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "foo_len" (RIP.Ptr Foo_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"foo_len")

instance FLAM.Offset Foo_bar Foo_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Foo = (FLAM.WithFlam Foo_bar) Foo_Aux

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Diff_Aux = Diff
  { diff_first :: RIP.CLong
    {- ^ __C declaration:__ @first@

         __defined at:__ @edge-cases\/flam.h 18:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , diff_second :: RIP.CChar
    {- ^ __C declaration:__ @second@

         __defined at:__ @edge-cases\/flam.h 19:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Diff_Aux where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Diff_Aux where

  readRaw =
    \ptr0 ->
          pure Diff
      <*> HasCField.readRaw (RIP.Proxy @"diff_first") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"diff_second") ptr0

instance Marshal.WriteRaw Diff_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               HasCField.writeRaw (RIP.Proxy @"diff_first") ptr0 diff_first2
            >> HasCField.writeRaw (RIP.Proxy @"diff_second") ptr0 diff_second3

deriving via Marshal.EquivStorable Diff_Aux instance RIP.Storable Diff_Aux

instance HasCField.HasCField Diff_Aux "diff_first" where

  type CFieldType Diff_Aux "diff_first" = RIP.CLong

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CLong
         ) => RIP.HasField "diff_first" (RIP.Ptr Diff_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"diff_first")

instance HasCField.HasCField Diff_Aux "diff_second" where

  type CFieldType Diff_Aux "diff_second" = RIP.CChar

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "diff_second" (RIP.Ptr Diff_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"diff_second")

instance FLAM.Offset RIP.CChar Diff_Aux where

  offset = \_ty0 -> 9

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Diff = (FLAM.WithFlam RIP.CChar) Diff_Aux

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
data Triplets_Aux = Triplets
  { triplets_len :: RIP.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 27:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Triplets_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Triplets_Aux where

  readRaw =
    \ptr0 ->
          pure Triplets
      <*> HasCField.readRaw (RIP.Proxy @"triplets_len") ptr0

instance Marshal.WriteRaw Triplets_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triplets triplets_len2 ->
            HasCField.writeRaw (RIP.Proxy @"triplets_len") ptr0 triplets_len2

deriving via Marshal.EquivStorable Triplets_Aux instance RIP.Storable Triplets_Aux

instance HasCField.HasCField Triplets_Aux "triplets_len" where

  type CFieldType Triplets_Aux "triplets_len" =
    RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "triplets_len" (RIP.Ptr Triplets_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"triplets_len")

instance FLAM.Offset ((CA.ConstantArray 3) RIP.CInt) Triplets_Aux where

  offset = \_ty0 -> 4

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
type Triplets =
  (FLAM.WithFlam ((CA.ConstantArray 3) RIP.CInt)) Triplets_Aux
