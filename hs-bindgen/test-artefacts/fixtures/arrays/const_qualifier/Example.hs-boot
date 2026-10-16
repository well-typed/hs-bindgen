{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data S

instance Eq S

instance BG.Generic S

instance Show S

instance IsA.IsArray S

data T

instance Eq T

instance BG.Generic T

instance Show T

instance IsA.IsArray T

data U

instance Eq U

instance BG.Generic U

instance Show U

instance IsA.IsArray U

instance Marshal.ReadRaw U

instance Marshal.StaticSize U

instance BG.Storable U

instance Marshal.WriteRaw U

data V

instance Eq V

instance BG.Generic V

instance Show V

instance IsA.IsArray V

instance Marshal.ReadRaw V

instance Marshal.StaticSize V

instance BG.Storable V

instance Marshal.WriteRaw V

data W

instance Eq W

instance BG.Generic W

instance Show W

instance IsA.IsArray W

instance Marshal.ReadRaw W

instance Marshal.StaticSize W

instance BG.Storable W

instance Marshal.WriteRaw W

data X

instance Eq X

instance BG.Generic X

instance Show X

instance IsA.IsArray X

instance Marshal.ReadRaw X

instance Marshal.StaticSize X

instance BG.Storable X

instance Marshal.WriteRaw X
