{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Support as BG

data T

instance BG.Generic T

instance BG.ToFunPtr T

instance BG.FromFunPtr T
