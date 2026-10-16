{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

data A

instance Eq A

instance BG.Generic A

instance Ord A

instance Read A

instance Show A

instance BG.Bitfield A

instance BG.Bits A

instance Bounded A

instance Enum A

instance BG.FiniteBits A

instance BG.HasFFIType A

instance Integral A

instance BG.Ix A

instance Num A

instance BG.Prim A

instance Marshal.ReadRaw A

instance Real A

instance Marshal.StaticSize A

instance BG.Storable A

instance Marshal.WriteRaw A

data Bool'

instance Eq Bool'

instance BG.Generic Bool'

instance Ord Bool'

instance Read Bool'

instance Show Bool'

instance BG.Bitfield Bool'

instance BG.Bits Bool'

instance Bounded Bool'

instance Enum Bool'

instance BG.FiniteBits Bool'

instance BG.HasFFIType Bool'

instance Integral Bool'

instance BG.Ix Bool'

instance Num Bool'

instance BG.Prim Bool'

instance Marshal.ReadRaw Bool'

instance Real Bool'

instance Marshal.StaticSize Bool'

instance BG.Storable Bool'

instance Marshal.WriteRaw Bool'
