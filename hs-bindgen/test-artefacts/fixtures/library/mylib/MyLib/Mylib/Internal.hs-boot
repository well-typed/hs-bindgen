{-# LANGUAGE NoImplicitPrelude #-}

module MyLib.Mylib.Internal where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Internal_state_t

instance Eq Internal_state_t

instance BG.Generic Internal_state_t

instance Show Internal_state_t

instance Marshal.StaticSize Internal_state_t

instance Marshal.ReadRaw Internal_state_t

instance Marshal.WriteRaw Internal_state_t

instance BG.Storable Internal_state_t

instance Struct.IsStruct Internal_state_t
