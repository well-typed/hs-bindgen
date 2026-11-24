{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AllOutputKinds.Cycle_b where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Ord, Show)

cYCLE_B_LIMIT :: BG.CInt

data Cycle_b_callback_Aux

instance BG.Generic Cycle_b_callback_Aux

data Cycle_b_callback

instance Eq Cycle_b_callback

instance BG.Generic Cycle_b_callback

instance Ord Cycle_b_callback

instance Show Cycle_b_callback

instance BG.HasFFIType Cycle_b_callback

instance Marshal.ReadRaw Cycle_b_callback

instance Marshal.StaticSize Cycle_b_callback

instance BG.Storable Cycle_b_callback

instance Marshal.WriteRaw Cycle_b_callback

data Cycle_b

instance Eq Cycle_b

instance BG.Generic Cycle_b

instance Show Cycle_b

instance Marshal.StaticSize Cycle_b

instance Marshal.ReadRaw Cycle_b

instance Marshal.WriteRaw Cycle_b

instance BG.Storable Cycle_b

instance Struct.IsStruct Cycle_b
