{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data A

instance Eq A

instance BG.Generic A

instance Show A

instance Marshal.StaticSize A

instance Marshal.ReadRaw A

instance Marshal.WriteRaw A

instance BG.Storable A

instance Struct.IsStruct A

data Struct1

instance Eq Struct1

instance BG.Generic Struct1

instance Show Struct1

instance Marshal.StaticSize Struct1

instance Marshal.ReadRaw Struct1

instance Marshal.WriteRaw Struct1

instance BG.Storable Struct1

instance Struct.IsStruct Struct1

data B_s

instance Eq B_s

instance BG.Generic B_s

instance Show B_s

instance Marshal.StaticSize B_s

instance Marshal.ReadRaw B_s

instance Marshal.WriteRaw B_s

instance BG.Storable B_s

instance Struct.IsStruct B_s

data Struct2_s

instance Eq Struct2_s

instance BG.Generic Struct2_s

instance Show Struct2_s

instance Marshal.StaticSize Struct2_s

instance Marshal.ReadRaw Struct2_s

instance Marshal.WriteRaw Struct2_s

instance BG.Storable Struct2_s

instance Struct.IsStruct Struct2_s

data C

instance Eq C

instance BG.Generic C

instance Show C

instance Marshal.StaticSize C

instance Marshal.ReadRaw C

instance Marshal.WriteRaw C

instance BG.Storable C

instance Struct.IsStruct C

data Struct3

instance Eq Struct3

instance BG.Generic Struct3

instance Show Struct3

instance Marshal.StaticSize Struct3

instance Marshal.ReadRaw Struct3

instance Marshal.WriteRaw Struct3

instance BG.Storable Struct3

instance Struct.IsStruct Struct3

data D

instance Eq D

instance BG.Generic D

instance Show D

instance Marshal.StaticSize D

instance Marshal.ReadRaw D

instance Marshal.WriteRaw D

instance BG.Storable D

instance Struct.IsStruct D

data Struct4

instance Eq Struct4

instance BG.Generic Struct4

instance Show Struct4

instance Marshal.StaticSize Struct4

instance Marshal.ReadRaw Struct4

instance Marshal.WriteRaw Struct4

instance BG.Storable Struct4

instance Struct.IsStruct Struct4

data E_s

instance Eq E_s

instance BG.Generic E_s

instance Show E_s

instance Marshal.StaticSize E_s

instance Marshal.ReadRaw E_s

instance Marshal.WriteRaw E_s

instance BG.Storable E_s

instance Struct.IsStruct E_s

data Struct5_s

instance Eq Struct5_s

instance BG.Generic Struct5_s

instance Show Struct5_s

instance Marshal.StaticSize Struct5_s

instance Marshal.ReadRaw Struct5_s

instance Marshal.WriteRaw Struct5_s

instance BG.Storable Struct5_s

instance Struct.IsStruct Struct5_s

data F

instance Eq F

instance BG.Generic F

instance Show F

instance Marshal.StaticSize F

instance Marshal.ReadRaw F

instance Marshal.WriteRaw F

instance BG.Storable F

instance Struct.IsStruct F

data Typedef1

instance Eq Typedef1

instance BG.Generic Typedef1

instance Show Typedef1

instance Marshal.StaticSize Typedef1

instance Marshal.ReadRaw Typedef1

instance Marshal.WriteRaw Typedef1

instance BG.Storable Typedef1

instance Struct.IsStruct Typedef1

data G

instance Eq G

instance BG.Generic G

instance Show G

instance Marshal.StaticSize G

instance Marshal.ReadRaw G

instance Marshal.WriteRaw G

instance BG.Storable G

instance Struct.IsStruct G

data Typedef2

instance Eq Typedef2

instance BG.Generic Typedef2

instance Show Typedef2

instance Marshal.StaticSize Typedef2

instance Marshal.ReadRaw Typedef2

instance Marshal.WriteRaw Typedef2

instance BG.Storable Typedef2

instance Struct.IsStruct Typedef2

data H

instance Eq H

instance BG.Generic H

instance Show H

instance Marshal.StaticSize H

instance Marshal.ReadRaw H

instance Marshal.WriteRaw H

instance BG.Storable H

instance Struct.IsStruct H

data Typedef3

instance Eq Typedef3

instance BG.Generic Typedef3

instance Show Typedef3

instance Marshal.StaticSize Typedef3

instance Marshal.ReadRaw Typedef3

instance Marshal.WriteRaw Typedef3

instance BG.Storable Typedef3

instance Struct.IsStruct Typedef3
