{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import Prelude (Eq, Show)

data Linked_list_A_t

instance Eq Linked_list_A_t

instance BG.Generic Linked_list_A_t

instance Show Linked_list_A_t

instance Marshal.StaticSize Linked_list_A_t

instance Marshal.ReadRaw Linked_list_A_t

instance Marshal.WriteRaw Linked_list_A_t

instance BG.Storable Linked_list_A_t

instance Struct.IsStruct Linked_list_A_t

data Linked_list_B_t

instance Eq Linked_list_B_t

instance BG.Generic Linked_list_B_t

instance Show Linked_list_B_t

instance Marshal.StaticSize Linked_list_B_t

instance Marshal.ReadRaw Linked_list_B_t

instance Marshal.WriteRaw Linked_list_B_t

instance BG.Storable Linked_list_B_t

instance Struct.IsStruct Linked_list_B_t
