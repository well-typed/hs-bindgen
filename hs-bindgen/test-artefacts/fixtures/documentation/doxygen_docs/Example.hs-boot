{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.Support as BG

data Size_type

data Opaque_struct

data Opaque_union

data Color_enum

data Event_callback_t_Aux

data Event_callback_t

data Config_t

data Status_code_t

data Data_union_t_as_parts

data Data_union_t

data Bitfield_t

data Processor_fn_t_Aux

data Processor_fn_t

data Filename_t

data Flexible_array_Aux

type Flexible_array =
  FLAM.WithFlam BG.CInt Flexible_array_Aux

data Dyn_array_t

data Multi_untagged_t_pos

data Multi_untagged_t_dim

data Multi_untagged_t

data Named_inner

data Named_outer

data Deep_mid_untagged_field

data Deep_mid

data Deep_outer

data Unnamed_field_t_anon'ua

data Unnamed_field_t

data Api_version_t
