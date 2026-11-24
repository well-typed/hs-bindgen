{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BootFiles.B where

import qualified C.Expr.HostPlatform
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

b_VALUE :: BG.CInt

b_FUNCTION :: forall a0. C.Expr.HostPlatform.Add (C.Expr.HostPlatform.MultRes a0 BG.CInt) BG.CInt => C.Expr.HostPlatform.Mult a0 BG.CInt => a0 -> C.Expr.HostPlatform.AddRes (C.Expr.HostPlatform.MultRes a0 BG.CInt) BG.CInt

data Peer_enum

instance Eq Peer_enum

instance BG.Generic Peer_enum

instance Ord Peer_enum

instance Read Peer_enum

instance Show Peer_enum

instance BG.HasFFIType Peer_enum

instance BG.Prim Peer_enum

instance Marshal.ReadRaw Peer_enum

instance Marshal.StaticSize Peer_enum

instance BG.Storable Peer_enum

instance Marshal.WriteRaw Peer_enum

data B

instance Eq B

instance BG.Generic B

instance Show B

instance Marshal.StaticSize B

instance Marshal.ReadRaw B

instance Marshal.WriteRaw B

instance BG.Storable B

instance Struct.IsStruct B
