{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data A

instance Eq A

instance BG.Generic A

instance Show A

instance IsA.IsArray A

data B

instance Eq B

instance BG.Generic B

instance Show B

instance IsA.IsArray B

data E

instance BG.Generic E
