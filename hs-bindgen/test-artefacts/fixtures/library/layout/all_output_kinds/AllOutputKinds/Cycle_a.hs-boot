{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AllOutputKinds.Cycle_a where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

cYCLE_A_LIMIT :: BG.CInt

data Cycle_a

instance Eq Cycle_a

instance BG.Generic Cycle_a

instance Show Cycle_a

instance Marshal.StaticSize Cycle_a

instance Marshal.ReadRaw Cycle_a

instance Marshal.WriteRaw Cycle_a

instance BG.Storable Cycle_a

instance Struct.IsStruct Cycle_a
