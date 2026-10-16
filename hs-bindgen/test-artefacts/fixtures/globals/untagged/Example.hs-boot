{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Ord, Read, Show)

data UntaggedPoint

instance Eq UntaggedPoint

instance BG.Generic UntaggedPoint

instance Show UntaggedPoint

instance Marshal.StaticSize UntaggedPoint

instance Marshal.ReadRaw UntaggedPoint

instance Marshal.WriteRaw UntaggedPoint

instance BG.Storable UntaggedPoint

instance Struct.IsStruct UntaggedPoint

data UntaggedPair

instance Eq UntaggedPair

instance BG.Generic UntaggedPair

instance Show UntaggedPair

instance Marshal.StaticSize UntaggedPair

instance Marshal.ReadRaw UntaggedPair

instance Marshal.WriteRaw UntaggedPair

instance BG.Storable UntaggedPair

instance Struct.IsStruct UntaggedPair

data UntaggedEnum

instance Eq UntaggedEnum

instance BG.Generic UntaggedEnum

instance Ord UntaggedEnum

instance BG.HasFFIType UntaggedEnum

instance Marshal.StaticSize UntaggedEnum

instance Marshal.ReadRaw UntaggedEnum

instance Marshal.WriteRaw UntaggedEnum

instance BG.Storable UntaggedEnum

instance BG.Prim UntaggedEnum

instance CEnum.CEnum UntaggedEnum

instance CEnum.SequentialCEnum UntaggedEnum

instance Show UntaggedEnum

instance Read UntaggedEnum

data UntaggedEnumCoords

instance Eq UntaggedEnumCoords

instance BG.Generic UntaggedEnumCoords

instance Ord UntaggedEnumCoords

instance BG.HasFFIType UntaggedEnumCoords

instance Marshal.StaticSize UntaggedEnumCoords

instance Marshal.ReadRaw UntaggedEnumCoords

instance Marshal.WriteRaw UntaggedEnumCoords

instance BG.Storable UntaggedEnumCoords

instance BG.Prim UntaggedEnumCoords

instance CEnum.CEnum UntaggedEnumCoords

instance Show UntaggedEnumCoords

instance Read UntaggedEnumCoords

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance BG.HasFFIType A

instance Marshal.StaticSize A

instance Marshal.ReadRaw A

instance Marshal.WriteRaw A

instance BG.Storable A

instance BG.Prim A

instance CEnum.CEnum A

instance CEnum.SequentialCEnum A

instance Show A

instance Read A

data B

instance BG.Generic B

instance Marshal.StaticSize B

instance Marshal.ReadRaw B

instance Marshal.WriteRaw B

instance BG.Storable B

instance Union.IsUnion B

data C

instance Eq C

instance BG.Generic C

instance Show C

instance Marshal.StaticSize C

instance Marshal.ReadRaw C

instance Marshal.WriteRaw C

instance BG.Storable C

instance Struct.IsStruct C
