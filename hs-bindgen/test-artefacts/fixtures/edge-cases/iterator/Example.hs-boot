{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Support as BG

data Toggle_Aux

instance BG.Generic Toggle_Aux

instance BG.ToFunPtr Toggle_Aux

instance BG.FromFunPtr Toggle_Aux

data Toggle

instance BG.Generic Toggle

instance BG.HasFFIType Toggle

data Counter_Aux

instance BG.Generic Counter_Aux

instance BG.ToFunPtr Counter_Aux

instance BG.FromFunPtr Counter_Aux

data Counter

instance BG.Generic Counter

instance BG.HasFFIType Counter

data VarCounter_Aux

instance BG.Generic VarCounter_Aux

instance BG.ToFunPtr VarCounter_Aux

instance BG.FromFunPtr VarCounter_Aux

data VarCounter

instance BG.Generic VarCounter

instance BG.HasFFIType VarCounter
