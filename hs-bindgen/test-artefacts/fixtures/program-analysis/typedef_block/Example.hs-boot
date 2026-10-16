{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Blk_struct

instance Eq Blk_struct

instance BG.Generic Blk_struct

instance Show Blk_struct

instance Marshal.StaticSize Blk_struct

instance Marshal.ReadRaw Blk_struct

instance Marshal.WriteRaw Blk_struct

instance BG.Storable Blk_struct

instance Struct.IsStruct Blk_struct

data Blk_Aux

instance BG.Generic Blk_Aux

data Blk

instance BG.Generic Blk

instance BG.HasFFIType Blk

data Blkarg_struct

instance Eq Blkarg_struct

instance BG.Generic Blkarg_struct

instance Show Blkarg_struct

instance Marshal.StaticSize Blkarg_struct

instance Marshal.ReadRaw Blkarg_struct

instance Marshal.WriteRaw Blkarg_struct

instance BG.Storable Blkarg_struct

instance Struct.IsStruct Blkarg_struct

data Blkarg_Aux

instance BG.Generic Blkarg_Aux

data Blkarg

instance BG.Generic Blkarg

instance BG.HasFFIType Blkarg
