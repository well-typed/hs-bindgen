{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Read, Show)

data First

instance Eq First

instance BG.Generic First

instance Ord First

instance BG.HasFFIType First

instance Marshal.StaticSize First

instance Marshal.ReadRaw First

instance Marshal.WriteRaw First

instance BG.Storable First

instance BG.Prim First

instance CEnum.CEnum First

instance CEnum.SequentialCEnum First

instance Show First

instance Read First

data Second

instance Eq Second

instance BG.Generic Second

instance Ord Second

instance BG.HasFFIType Second

instance Marshal.StaticSize Second

instance Marshal.ReadRaw Second

instance Marshal.WriteRaw Second

instance BG.Storable Second

instance BG.Prim Second

instance CEnum.CEnum Second

instance CEnum.SequentialCEnum Second

instance Show Second

instance Read Second

data Same

instance Eq Same

instance BG.Generic Same

instance Ord Same

instance BG.HasFFIType Same

instance Marshal.StaticSize Same

instance Marshal.ReadRaw Same

instance Marshal.WriteRaw Same

instance BG.Storable Same

instance BG.Prim Same

instance CEnum.CEnum Same

instance CEnum.SequentialCEnum Same

instance Show Same

instance Read Same

data Nonseq

instance Eq Nonseq

instance BG.Generic Nonseq

instance Ord Nonseq

instance BG.HasFFIType Nonseq

instance Marshal.StaticSize Nonseq

instance Marshal.ReadRaw Nonseq

instance Marshal.WriteRaw Nonseq

instance BG.Storable Nonseq

instance BG.Prim Nonseq

instance CEnum.CEnum Nonseq

instance Show Nonseq

instance Read Nonseq

data Packed

instance Eq Packed

instance BG.Generic Packed

instance Ord Packed

instance BG.HasFFIType Packed

instance Marshal.StaticSize Packed

instance Marshal.ReadRaw Packed

instance Marshal.WriteRaw Packed

instance BG.Storable Packed

instance BG.Prim Packed

instance CEnum.CEnum Packed

instance CEnum.SequentialCEnum Packed

instance Show Packed

instance Read Packed

data EnumA

instance Eq EnumA

instance BG.Generic EnumA

instance Ord EnumA

instance BG.HasFFIType EnumA

instance Marshal.StaticSize EnumA

instance Marshal.ReadRaw EnumA

instance Marshal.WriteRaw EnumA

instance BG.Storable EnumA

instance BG.Prim EnumA

instance CEnum.CEnum EnumA

instance CEnum.SequentialCEnum EnumA

instance Show EnumA

instance Read EnumA

data EnumB

instance Eq EnumB

instance BG.Generic EnumB

instance Ord EnumB

instance BG.HasFFIType EnumB

instance Marshal.StaticSize EnumB

instance Marshal.ReadRaw EnumB

instance Marshal.WriteRaw EnumB

instance BG.Storable EnumB

instance BG.Prim EnumB

instance CEnum.CEnum EnumB

instance CEnum.SequentialCEnum EnumB

instance Show EnumB

instance Read EnumB

data EnumC

instance Eq EnumC

instance BG.Generic EnumC

instance Ord EnumC

instance BG.HasFFIType EnumC

instance Marshal.StaticSize EnumC

instance Marshal.ReadRaw EnumC

instance Marshal.WriteRaw EnumC

instance BG.Storable EnumC

instance BG.Prim EnumC

instance CEnum.CEnum EnumC

instance CEnum.SequentialCEnum EnumC

instance Show EnumC

instance Read EnumC

data EnumD_t

instance Eq EnumD_t

instance BG.Generic EnumD_t

instance Ord EnumD_t

instance BG.HasFFIType EnumD_t

instance Marshal.StaticSize EnumD_t

instance Marshal.ReadRaw EnumD_t

instance Marshal.WriteRaw EnumD_t

instance BG.Storable EnumD_t

instance BG.Prim EnumD_t

instance CEnum.CEnum EnumD_t

instance CEnum.SequentialCEnum EnumD_t

instance Show EnumD_t

instance Read EnumD_t
