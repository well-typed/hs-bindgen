{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Support as BG

data T_Aux

instance BG.Generic T_Aux

instance BG.ToFunPtr T_Aux

instance BG.FromFunPtr T_Aux

data T

instance BG.Generic T

instance BG.HasFFIType T
