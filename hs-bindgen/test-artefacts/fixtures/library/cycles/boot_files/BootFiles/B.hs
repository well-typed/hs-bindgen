{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module BootFiles.B
    ( BootFiles.B.b_VALUE
    , BootFiles.B.b_FUNCTION
    , BootFiles.B.Peer_enum(..)
    , BootFiles.B.B(..)
    )
  where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified BootFiles.A
import Prelude ((<*>), (>>), Eq, Int, Ord, Read, Show, pure, type (~))

{-| __C declaration:__ @macro B_VALUE@

    __defined at:__ @boot_files\/b.h 8:9@

    __exported by:__ @boot_files.h@
-}
b_VALUE :: BG.CInt
b_VALUE =
  (C.Expr.HostPlatform./) BootFiles.A.a_VALUE (2 :: BG.CInt)

{-| __C declaration:__ @macro B_FUNCTION@

    __defined at:__ @boot_files\/b.h 9:9@

    __exported by:__ @boot_files.h@
-}
b_FUNCTION :: forall a0. C.Expr.HostPlatform.Add (C.Expr.HostPlatform.MultRes a0 BG.CInt) BG.CInt => C.Expr.HostPlatform.Mult a0 BG.CInt => a0 -> C.Expr.HostPlatform.AddRes (C.Expr.HostPlatform.MultRes a0 BG.CInt) BG.CInt
b_FUNCTION =
  \n0 ->
    (C.Expr.HostPlatform.+) (BootFiles.A.a_FUNCTION n0) (1 :: BG.CInt)

{-| __C declaration:__ @peer_enum@

    __defined at:__ @boot_files\/b.h 13:21@

    __exported by:__ @boot_files.h@
-}
newtype Peer_enum = Peer_enum
  { unwrapPeer_enum :: BootFiles.A.A_enum
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.HasFFIType
    , BG.Prim
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BootFiles.A.A_enum
         ) => BG.CompatHasField.HasField "unwrapPeer_enum" Peer_enum ty where

  hasField =
    \x0 ->
      (\y1 ->
         Peer_enum {unwrapPeer_enum = y1}, BG.getField @"unwrapPeer_enum" x0)

instance ( ty ~ BootFiles.A.A_enum
         ) => BG.HasField "unwrapPeer_enum" (BG.Ptr Peer_enum) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapPeer_enum")

instance HasCField.HasCField Peer_enum "unwrapPeer_enum" where

  type CFieldType Peer_enum "unwrapPeer_enum" =
    BootFiles.A.A_enum

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct b@

    __defined at:__ @boot_files\/b.h 15:8@

    __exported by:__ @boot_files.h@
-}
data B = B
  { b_a :: BG.Ptr BootFiles.A.A
    {- ^ __C declaration:__ @a@

         __defined at:__ @boot_files\/b.h 16:13@

         __exported by:__ @boot_files.h@
    -}
  , b_e :: Peer_enum
    {- ^ __C declaration:__ @e@

         __defined at:__ @boot_files\/b.h 17:13@

         __exported by:__ @boot_files.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize B where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw B where

  readRaw =
    \ptr0 ->
          pure B
      <*> HasCField.readRaw (BG.Proxy @"b_a") ptr0
      <*> HasCField.readRaw (BG.Proxy @"b_e") ptr0

instance Marshal.WriteRaw B where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          B b_a2 b_e3 ->
               HasCField.writeRaw (BG.Proxy @"b_a") ptr0 b_a2
            >> HasCField.writeRaw (BG.Proxy @"b_e") ptr0 b_e3

deriving via Marshal.EquivStorable B instance BG.Storable B

deriving via Struct.IsStructViaReadRaw B instance Struct.IsStruct B

{-| __C declaration:__ @a@

    __defined at:__ @boot_files\/b.h 16:13@

    __exported by:__ @boot_files.h@
-}
instance ( ty ~ BG.Ptr BootFiles.A.A
         ) => BG.CompatHasField.HasField "b_a" B ty where

  hasField =
    \x0 ->
      (\y1 ->
         B {b_a = y1, b_e = BG.getField @"b_e" x0}, BG.getField @"b_a" x0)

instance ( ty ~ BG.Ptr BootFiles.A.A
         ) => BG.HasField "b_a" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_a")

instance HasCField.HasCField B "b_a" where

  type CFieldType B "b_a" = BG.Ptr BootFiles.A.A

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @e@

    __defined at:__ @boot_files\/b.h 17:13@

    __exported by:__ @boot_files.h@
-}
instance (ty ~ Peer_enum) => BG.CompatHasField.HasField "b_e" B ty where

  hasField =
    \x0 ->
      (\y1 ->
         B {b_e = y1, b_a = BG.getField @"b_a" x0}, BG.getField @"b_e" x0)

instance (ty ~ Peer_enum) => BG.HasField "b_e" (BG.Ptr B) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"b_e")

instance HasCField.HasCField B "b_e" where

  type CFieldType B "b_e" = Peer_enum

  offset# = \_ -> \_ -> 8
