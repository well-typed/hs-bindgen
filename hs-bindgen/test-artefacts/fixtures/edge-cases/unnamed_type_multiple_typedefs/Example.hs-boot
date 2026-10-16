{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

data Point1a

instance Eq Point1a

instance BG.Generic Point1a

instance Show Point1a

instance Marshal.StaticSize Point1a

instance Marshal.ReadRaw Point1a

instance Marshal.WriteRaw Point1a

instance BG.Storable Point1a

instance Struct.IsStruct Point1a

data Point1b

instance Eq Point1b

instance BG.Generic Point1b

instance Show Point1b

instance Marshal.ReadRaw Point1b

instance Marshal.StaticSize Point1b

instance BG.Storable Point1b

instance Marshal.WriteRaw Point1b

data Point2a

instance Eq Point2a

instance BG.Generic Point2a

instance Show Point2a

instance Marshal.StaticSize Point2a

instance Marshal.ReadRaw Point2a

instance Marshal.WriteRaw Point2a

instance BG.Storable Point2a

instance Struct.IsStruct Point2a

data Point2b

instance Eq Point2b

instance BG.Generic Point2b

instance Ord Point2b

instance Show Point2b

instance BG.HasFFIType Point2b

instance Marshal.ReadRaw Point2b

instance Marshal.StaticSize Point2b

instance BG.Storable Point2b

instance Marshal.WriteRaw Point2b

data Point3a_Aux

instance Eq Point3a_Aux

instance BG.Generic Point3a_Aux

instance Show Point3a_Aux

instance Marshal.StaticSize Point3a_Aux

instance Marshal.ReadRaw Point3a_Aux

instance Marshal.WriteRaw Point3a_Aux

instance BG.Storable Point3a_Aux

instance Struct.IsStruct Point3a_Aux

data Point3a

instance Eq Point3a

instance BG.Generic Point3a

instance Ord Point3a

instance Show Point3a

instance BG.HasFFIType Point3a

instance Marshal.ReadRaw Point3a

instance Marshal.StaticSize Point3a

instance BG.Storable Point3a

instance Marshal.WriteRaw Point3a

data Point3b

instance Eq Point3b

instance BG.Generic Point3b

instance Ord Point3b

instance Show Point3b

instance BG.HasFFIType Point3b

instance Marshal.ReadRaw Point3b

instance Marshal.StaticSize Point3b

instance BG.Storable Point3b

instance Marshal.WriteRaw Point3b
