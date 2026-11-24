{-# LANGUAGE NoImplicitPrelude #-}

module TypedefAndRawCallbacks.Raw_callbacks where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Loop

instance Eq Loop

instance BG.Generic Loop

instance Show Loop

instance Marshal.StaticSize Loop

instance Marshal.ReadRaw Loop

instance Marshal.WriteRaw Loop

instance BG.Storable Loop

instance Struct.IsStruct Loop
