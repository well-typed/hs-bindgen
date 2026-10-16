{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Union as Union
import Prelude (Eq, Show)

data Dim2

instance Eq Dim2

instance BG.Generic Dim2

instance Show Dim2

instance Marshal.StaticSize Dim2

instance Marshal.ReadRaw Dim2

instance Marshal.WriteRaw Dim2

instance BG.Storable Dim2

instance Struct.IsStruct Dim2

data Dim3

instance Eq Dim3

instance BG.Generic Dim3

instance Show Dim3

instance Marshal.StaticSize Dim3

instance Marshal.ReadRaw Dim3

instance Marshal.WriteRaw Dim3

instance BG.Storable Dim3

instance Struct.IsStruct Dim3

data DimPayload

instance BG.Generic DimPayload

instance Marshal.StaticSize DimPayload

instance Marshal.ReadRaw DimPayload

instance Marshal.WriteRaw DimPayload

instance BG.Storable DimPayload

instance Union.IsUnion DimPayload

data Dim

instance BG.Generic Dim

instance Marshal.StaticSize Dim

instance Marshal.ReadRaw Dim

instance Marshal.WriteRaw Dim

instance BG.Storable Dim

instance Struct.IsStruct Dim

data DimPayloadB

instance BG.Generic DimPayloadB

instance Marshal.StaticSize DimPayloadB

instance Marshal.ReadRaw DimPayloadB

instance Marshal.WriteRaw DimPayloadB

instance BG.Storable DimPayloadB

instance Union.IsUnion DimPayloadB

data DimB

instance BG.Generic DimB

instance Marshal.StaticSize DimB

instance Marshal.ReadRaw DimB

instance Marshal.WriteRaw DimB

instance BG.Storable DimB

instance Struct.IsStruct DimB

data UntaggedA_xy

instance Eq UntaggedA_xy

instance BG.Generic UntaggedA_xy

instance Show UntaggedA_xy

instance Marshal.StaticSize UntaggedA_xy

instance Marshal.ReadRaw UntaggedA_xy

instance Marshal.WriteRaw UntaggedA_xy

instance BG.Storable UntaggedA_xy

instance Struct.IsStruct UntaggedA_xy

data UntaggedA_polar

instance Eq UntaggedA_polar

instance BG.Generic UntaggedA_polar

instance Show UntaggedA_polar

instance Marshal.StaticSize UntaggedA_polar

instance Marshal.ReadRaw UntaggedA_polar

instance Marshal.WriteRaw UntaggedA_polar

instance BG.Storable UntaggedA_polar

instance Struct.IsStruct UntaggedA_polar

data UntaggedA

instance BG.Generic UntaggedA

instance Marshal.StaticSize UntaggedA

instance Marshal.ReadRaw UntaggedA

instance Marshal.WriteRaw UntaggedA

instance BG.Storable UntaggedA

instance Union.IsUnion UntaggedA
