{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data T_Aux

instance BG.Generic T_Aux

instance BG.ToFunPtr T_Aux

instance BG.FromFunPtr T_Aux

data T

instance Eq T

instance BG.Generic T

instance Show T

instance IsA.IsArray T

data U_Aux

instance BG.Generic U_Aux

instance BG.ToFunPtr U_Aux

instance BG.FromFunPtr U_Aux

data U

instance Eq U

instance BG.Generic U

instance Show U

instance IsA.IsArray U

instance Marshal.ReadRaw U

instance Marshal.StaticSize U

instance BG.Storable U

instance Marshal.WriteRaw U
