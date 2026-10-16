{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data T_Aux

instance BG.Generic T_Aux

instance BG.ToFunPtr T_Aux

instance BG.FromFunPtr T_Aux

data T

instance Eq T

instance BG.Generic T

instance Ord T

instance Show T

instance BG.HasFFIType T

instance Marshal.ReadRaw T

instance Marshal.StaticSize T

instance BG.Storable T

instance Marshal.WriteRaw T
