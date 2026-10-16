{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data Foo

instance Eq Foo

instance BG.Generic Foo

instance Ord Foo

instance BG.HasFFIType Foo

instance Marshal.StaticSize Foo

instance Marshal.ReadRaw Foo

instance Marshal.WriteRaw Foo

instance BG.Storable Foo

instance BG.Prim Foo

instance CEnum.CEnum Foo

instance CEnum.SequentialCEnum Foo

instance Show Foo

instance Read Foo

data ElementaryParticleType

instance Eq ElementaryParticleType

instance BG.Generic ElementaryParticleType

instance Ord ElementaryParticleType

instance BG.HasFFIType ElementaryParticleType

instance Marshal.StaticSize ElementaryParticleType

instance Marshal.ReadRaw ElementaryParticleType

instance Marshal.WriteRaw ElementaryParticleType

instance BG.Storable ElementaryParticleType

instance BG.Prim ElementaryParticleType

instance CEnum.CEnum ElementaryParticleType

instance CEnum.SequentialCEnum ElementaryParticleType

instance Show ElementaryParticleType

instance Read ElementaryParticleType
