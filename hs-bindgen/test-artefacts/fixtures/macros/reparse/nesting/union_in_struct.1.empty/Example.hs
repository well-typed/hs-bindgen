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
    ( Example.T1_x(..)
    , Example.T1(..)
    , Example.T2(..)
    , Example.T2_x(..)
    , Example.T3(..)
    , Example.T3_x(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @union \@T1_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:10@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
newtype T1_x = T1_x
  { unwrapT1_x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T1_x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T1_x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T1_x

deriving via Marshal.EquivStorable T1_x instance BG.Storable T1_x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T1_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t1_x_x" T1_x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t1_x_x" T1_x ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t1_x_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t1_x_x" (BG.Ptr T1_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x_x")

instance HasCField.HasCField T1_x "t1_x_x" where

  type CFieldType T1_x "t1_x_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T1@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
data T1 = T1
  { t1_x :: T1_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:32@

         __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
    -}
  }
  deriving stock (BG.Generic)

instance Marshal.StaticSize T1 where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw T1 where

  readRaw =
    \ptr0 ->
          pure T1
      <*> HasCField.readRaw (BG.Proxy @"t1_x") ptr0

instance Marshal.WriteRaw T1 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T1 t1_x2 ->
            HasCField.writeRaw (BG.Proxy @"t1_x") ptr0 t1_x2

deriving via Marshal.EquivStorable T1 instance BG.Storable T1

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 3:32@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ T1_x) => BG.CompatHasField.HasField "t1_x" T1 ty where

  hasField =
    \x0 ->
      (\y1 -> T1 {t1_x = y1}, BG.getField @"t1_x" x0)

instance (ty ~ T1_x) => BG.HasField "t1_x" (BG.Ptr T1) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t1_x")

instance HasCField.HasCField T1 "t1_x" where

  type CFieldType T1 "t1_x" = T1_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T2@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
data T2 = T2
  { t2_x :: BG.Ptr T2_x
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:32@

         __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw T2 where

  readRaw =
    \ptr0 ->
          pure T2
      <*> HasCField.readRaw (BG.Proxy @"t2_x") ptr0

instance Marshal.WriteRaw T2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T2 t2_x2 ->
            HasCField.writeRaw (BG.Proxy @"t2_x") ptr0 t2_x2

deriving via Marshal.EquivStorable T2 instance BG.Storable T2

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:32@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.Ptr T2_x) => BG.CompatHasField.HasField "t2_x" T2 ty where

  hasField =
    \x0 ->
      (\y1 -> T2 {t2_x = y1}, BG.getField @"t2_x" x0)

instance (ty ~ BG.Ptr T2_x) => BG.HasField "t2_x" (BG.Ptr T2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_x")

instance HasCField.HasCField T2 "t2_x" where

  type CFieldType T2 "t2_x" = BG.Ptr T2_x

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T2_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:10@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
newtype T2_x = T2_x
  { unwrapT2_x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T2_x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T2_x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T2_x

deriving via Marshal.EquivStorable T2_x instance BG.Storable T2_x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T2_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t2_x_x" T2_x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 4:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t2_x_x" T2_x ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t2_x_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t2_x_x" (BG.Ptr T2_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t2_x_x")

instance HasCField.HasCField T2_x "t2_x_x" where

  type CFieldType T2_x "t2_x_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct \@T3@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:1@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
data T3 = T3
  { t3_x :: BG.Ptr (BG.Ptr T3_x)
    {- ^ __C declaration:__ @x@

         __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:32@

         __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize T3 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw T3 where

  readRaw =
    \ptr0 ->
          pure T3
      <*> HasCField.readRaw (BG.Proxy @"t3_x") ptr0

instance Marshal.WriteRaw T3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          T3 t3_x2 ->
            HasCField.writeRaw (BG.Proxy @"t3_x") ptr0 t3_x2

deriving via Marshal.EquivStorable T3 instance BG.Storable T3

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:32@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance ( ty ~ BG.Ptr (BG.Ptr T3_x)
         ) => BG.CompatHasField.HasField "t3_x" T3 ty where

  hasField =
    \x0 ->
      (\y1 -> T3 {t3_x = y1}, BG.getField @"t3_x" x0)

instance ( ty ~ BG.Ptr (BG.Ptr T3_x)
         ) => BG.HasField "t3_x" (BG.Ptr T3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_x")

instance HasCField.HasCField T3 "t3_x" where

  type CFieldType T3 "t3_x" = BG.Ptr (BG.Ptr T3_x)

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union \@T3_x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:10@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
newtype T3_x = T3_x
  { unwrapT3_x :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize T3_x

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw T3_x

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw T3_x

deriving via Marshal.EquivStorable T3_x instance BG.Storable T3_x

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion T3_x

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "t3_x_x" T3_x ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @x@

    __defined at:__ @macros\/reparse\/nesting\/union_in_struct.h 5:24@

    __exported by:__ @macros\/reparse\/nesting\/union_in_struct.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "t3_x_x" T3_x ty where

  hasField =
    \x0 -> (BG.setUnionPayload, BG.getField @"t3_x_x" x0)

instance (ty ~ BG.CInt) => BG.HasField "t3_x_x" (BG.Ptr T3_x) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"t3_x_x")

instance HasCField.HasCField T3_x "t3_x_x" where

  type CFieldType T3_x "t3_x_x" = BG.CInt

  offset# = \_ -> \_ -> 0
