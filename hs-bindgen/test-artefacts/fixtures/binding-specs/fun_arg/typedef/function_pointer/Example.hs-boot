{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data A_Aux

instance BG.Generic A_Aux

instance BG.ToFunPtr A_Aux

instance BG.FromFunPtr A_Aux

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Show A

instance BG.HasFFIType A

instance Marshal.ReadRaw A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data B

instance Eq B

instance BG.Generic B

instance Ord B

instance Show B

instance BG.HasFFIType B

instance Marshal.ReadRaw B

instance Marshal.StaticSize B

instance BG.Storable B

instance Marshal.WriteRaw B

data E

instance BG.Generic E

instance BG.HasFFIType E
