{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Support as BG

data MyFunction

instance BG.Generic MyFunction

instance BG.ToFunPtr MyFunction

instance BG.FromFunPtr MyFunction

data A

instance BG.Generic A

data B

instance BG.Generic B

data E

instance BG.Generic E
