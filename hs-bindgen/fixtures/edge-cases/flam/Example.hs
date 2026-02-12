{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FLAM
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Marshal
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Pascal_Aux = Pascal
  { pascal_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 3:9@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Pascal_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Pascal_Aux where

  readRaw =
    \ptr0 ->
          pure Pascal
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"pascal_len") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Pascal_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Pascal pascal_len2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"pascal_len") ptr0 pascal_len2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Pascal_Aux instance F.Storable Pascal_Aux

instance HsBindgen.Runtime.HasCField.HasCField Pascal_Aux "pascal_len" where

  type CFieldType Pascal_Aux "pascal_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "pascal_len" (Ptr.Ptr Pascal_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"pascal_len")

instance HsBindgen.Runtime.FLAM.Offset FC.CChar Pascal_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct pascal@

    __defined at:__ @edge-cases\/flam.h 2:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Pascal =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CChar) Pascal_Aux

{-| __C declaration:__ @struct \@foo_bar@

    __defined at:__ @edge-cases\/flam.h 10:2@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_bar = Foo_bar
  { foo_bar_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @edge-cases\/flam.h 11:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , foo_bar_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @edge-cases\/flam.h 12:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo_bar where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo_bar where

  readRaw =
    \ptr0 ->
          pure Foo_bar
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_bar_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_bar_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo_bar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo_bar foo_bar_x2 foo_bar_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_bar_x") ptr0 foo_bar_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_bar_y") ptr0 foo_bar_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo_bar instance F.Storable Foo_bar

instance HsBindgen.Runtime.HasCField.HasCField Foo_bar "foo_bar_x" where

  type CFieldType Foo_bar "foo_bar_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "foo_bar_x" (Ptr.Ptr Foo_bar) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_bar_x")

instance HsBindgen.Runtime.HasCField.HasCField Foo_bar "foo_bar_y" where

  type CFieldType Foo_bar "foo_bar_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance GHC.Records.HasField "foo_bar_y" (Ptr.Ptr Foo_bar) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_bar_y")

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Foo_Aux = Foo
  { foo_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 9:6@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Foo_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Foo_Aux where

  readRaw =
    \ptr0 ->
          pure Foo
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"foo_len") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Foo_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_len2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"foo_len") ptr0 foo_len2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Foo_Aux instance F.Storable Foo_Aux

instance HsBindgen.Runtime.HasCField.HasCField Foo_Aux "foo_len" where

  type CFieldType Foo_Aux "foo_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "foo_len" (Ptr.Ptr Foo_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"foo_len")

instance HsBindgen.Runtime.FLAM.Offset Foo_bar Foo_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct foo@

    __defined at:__ @edge-cases\/flam.h 8:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Foo =
  (HsBindgen.Runtime.FLAM.WithFlam Foo_bar) Foo_Aux

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
data Diff_Aux = Diff
  { diff_first :: FC.CLong
    {- ^ __C declaration:__ @first@

         __defined at:__ @edge-cases\/flam.h 18:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  , diff_second :: FC.CChar
    {- ^ __C declaration:__ @second@

         __defined at:__ @edge-cases\/flam.h 19:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Diff_Aux where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Diff_Aux where

  readRaw =
    \ptr0 ->
          pure Diff
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"diff_first") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"diff_second") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Diff_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Diff diff_first2 diff_second3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"diff_first") ptr0 diff_first2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"diff_second") ptr0 diff_second3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Diff_Aux instance F.Storable Diff_Aux

instance HsBindgen.Runtime.HasCField.HasCField Diff_Aux "diff_first" where

  type CFieldType Diff_Aux "diff_first" = FC.CLong

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "diff_first" (Ptr.Ptr Diff_Aux) (Ptr.Ptr FC.CLong) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"diff_first")

instance HsBindgen.Runtime.HasCField.HasCField Diff_Aux "diff_second" where

  type CFieldType Diff_Aux "diff_second" = FC.CChar

  offset# = \_ -> \_ -> 8

instance GHC.Records.HasField "diff_second" (Ptr.Ptr Diff_Aux) (Ptr.Ptr FC.CChar) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"diff_second")

instance HsBindgen.Runtime.FLAM.Offset FC.CChar Diff_Aux where

  offset = \_ty0 -> 9

{-| __C declaration:__ @struct diff@

    __defined at:__ @edge-cases\/flam.h 17:8@

    __exported by:__ @edge-cases\/flam.h@
-}
type Diff =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CChar) Diff_Aux

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
data Triplets_Aux = Triplets
  { triplets_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @edge-cases\/flam.h 27:7@

         __exported by:__ @edge-cases\/flam.h@
    -}
  }
  deriving stock (GHC.Generics.Generic, Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Triplets_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Triplets_Aux where

  readRaw =
    \ptr0 ->
          pure Triplets
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"triplets_len") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Triplets_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Triplets triplets_len2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"triplets_len") ptr0 triplets_len2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Triplets_Aux instance F.Storable Triplets_Aux

instance HsBindgen.Runtime.HasCField.HasCField Triplets_Aux "triplets_len" where

  type CFieldType Triplets_Aux "triplets_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance GHC.Records.HasField "triplets_len" (Ptr.Ptr Triplets_Aux) (Ptr.Ptr FC.CInt) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"triplets_len")

instance HsBindgen.Runtime.FLAM.Offset ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) Triplets_Aux where

  offset = \_ty0 -> 4

{-| The flexible array member is a multi-dimensional array of unknown size. In particular, it is a is an array of unknown size, where each element is of type length-3-array-of-int.

__C declaration:__ @struct triplets@

__defined at:__ @edge-cases\/flam.h 26:8@

__exported by:__ @edge-cases\/flam.h@
-}
type Triplets =
  (HsBindgen.Runtime.FLAM.WithFlam ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) Triplets_Aux
