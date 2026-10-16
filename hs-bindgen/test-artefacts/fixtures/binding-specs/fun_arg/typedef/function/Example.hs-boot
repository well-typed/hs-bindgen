{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Support as BG

data A

instance BG.Generic A

instance BG.ToFunPtr A

instance BG.FromFunPtr A

data B

instance BG.Generic B

data E

instance BG.Generic E
